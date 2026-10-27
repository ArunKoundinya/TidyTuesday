"""Email Tidy Tuesday subscribers about new analyses. Run after publishing:

    python3 scripts/notify.py              # announce analyses not announced before
    python3 scripts/notify.py --dry-run    # show what would be sent, send nothing
    python3 scripts/notify.py --test you@example.com [analysis-folder]   # one sample email, records nothing

The first run only records every existing analysis as announced (no emails).
Announced analyses are tracked in subscribe/announced.json (commit it).

Subscribers live on the Worker shared with Canvass And Analyze (list "tidytuesday"),
so this uses the same settings file, created by the CanvassAndAnalyze repo's
scripts/setup_subscribe_secrets.sh: ~/.config/canvass-and-analyze/notify.env
    WORKER_URL, ADMIN_TOKEN, GMAIL_ADDRESS, GMAIL_APP_PASSWORD
"""
import email.utils, json, os, smtplib, ssl, sys, time, urllib.request
from email.message import EmailMessage

sys.path.insert(0, os.path.dirname(__file__))
from email_templates import BRAND, SITE, analyses, new_week, post_info  # noqa: E402

LIST = json.load(open("subscribe/site.json")).get("list", "tidytuesday")
STATE = "subscribe/announced.json"
ENV = os.path.expanduser("~/.config/canvass-and-analyze/notify.env")


def settings():
    cfg = {}
    if os.path.exists(ENV):
        for line in open(ENV):
            if "=" in line and not line.lstrip().startswith("#"):
                k, v = line.strip().split("=", 1)
                cfg[k.strip()] = v.strip().strip("'\"")
    missing = [k for k in ("WORKER_URL", "ADMIN_TOKEN", "GMAIL_ADDRESS", "GMAIL_APP_PASSWORD") if not cfg.get(k)]
    if missing:
        sys.exit(f"notify.py: missing {', '.join(missing)} in {ENV}. "
                 "Run scripts/setup_subscribe_secrets.sh in the CanvassAndAnalyze repo first.")
    cfg["WORKER_URL"] = cfg["WORKER_URL"].rstrip("/")
    return cfg


def subscribers(cfg):
    req = urllib.request.Request(f"{cfg['WORKER_URL']}/admin/subscribers?list={LIST}",
                                 headers={"Authorization": "Bearer " + cfg["ADMIN_TOKEN"], "User-Agent": "notify.py"})
    with urllib.request.urlopen(req, timeout=30) as r:
        return json.load(r)["subscribers"]


def wait_until_live(url, minutes=5):
    for _ in range(minutes * 6):
        try:
            with urllib.request.urlopen(urllib.request.Request(url, method="HEAD"), timeout=15) as r:
                if r.status == 200:
                    return True
        except Exception:
            pass
        time.sleep(10)
    return False


def message(cfg, post, to, token):
    unsub_page = f"{SITE}unsubscribe.html?token={token}"
    one_click = f"{cfg['WORKER_URL']}/unsubscribe?token={token}"
    subject, html, text = new_week(post, unsub_page)
    m = EmailMessage()
    m["From"] = email.utils.formataddr((BRAND["from_name"], cfg["GMAIL_ADDRESS"]))
    m["To"] = to
    m["Subject"] = subject
    m["Date"] = email.utils.formatdate(localtime=True)
    m["List-Unsubscribe"] = f"<{one_click}>"
    m["List-Unsubscribe-Post"] = "List-Unsubscribe=One-Click"
    m.set_content(text)
    m.add_alternative(html, subtype="html")
    return m


def main(argv):
    dry = "--dry-run" in argv
    os.makedirs(os.path.dirname(STATE), exist_ok=True)

    if "--test" in argv:
        i = argv.index("--test")
        to = argv[i + 1]
        folder = argv[i + 2] if len(argv) > i + 2 and not argv[i + 2].startswith("--") else max(
            analyses(), key=lambda f: post_info(f)["date_iso"])
        cfg = settings()
        token = next((x["token"] for x in subscribers(cfg) if x["email"] == to.lower()), "TEST-TOKEN")  # real link if subscribed
        with smtplib.SMTP_SSL("smtp.gmail.com", 465, context=ssl.create_default_context()) as smtp:
            smtp.login(cfg["GMAIL_ADDRESS"], cfg["GMAIL_APP_PASSWORD"])
            smtp.send_message(message(cfg, post_info(folder), to, token))
        print(f"notify.py: sent a test email about '{folder}' to {to}" + (" (real unsubscribe link)" if token != "TEST-TOKEN" else ""))
        return

    if not os.path.exists(STATE):
        json.dump({"announced": analyses()}, open(STATE, "w"), indent=1)
        print(f"notify.py: first run. Recorded {len(analyses())} existing analyses as announced; no emails sent.")
        return

    announced = set(json.load(open(STATE))["announced"])
    new = [f for f in analyses() if f not in announced]
    if not new:
        print("notify.py: no new analyses to announce.")
        return
    posts = sorted((post_info(f) for f in new), key=lambda p: p["date_iso"])
    cfg = settings()
    subs = subscribers(cfg)
    print(f"notify.py: {len(posts)} new analysis(es), {len(subs)} subscriber(s).")
    if dry:
        for p in posts:
            print(f"  would send: {new_week(p, '')[0]}")
        return

    with smtplib.SMTP_SSL("smtp.gmail.com", 465, context=ssl.create_default_context()) as smtp:
        smtp.login(cfg["GMAIL_ADDRESS"], cfg["GMAIL_APP_PASSWORD"])
        for p in posts:
            if not wait_until_live(p["url"]):
                print(f"  skipped {p['slug']}: not live yet at {p['url']} (will retry next run)")
                continue
            sent = failed = 0
            for s in subs:
                try:
                    smtp.send_message(message(cfg, p, s["email"], s["token"]))
                    sent += 1
                except smtplib.SMTPRecipientsRefused:
                    failed += 1
                time.sleep(1)                                    # stay well inside Gmail's rate limits
            announced.add(p["slug"])
            json.dump({"announced": sorted(announced)}, open(STATE, "w"), indent=1)
            print(f"  announced {p['slug']}: sent {sent}" + (f", {failed} refused" if failed else ""))


if __name__ == "__main__":
    main(sys.argv[1:])
