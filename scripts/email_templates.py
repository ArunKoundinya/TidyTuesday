"""Branded emails for Tidy Tuesday subscribers.

Same layout as Canvass And Analyze's emails (cream page, white card, logo, one
button, an unsubscribe footer) in this site's colours: ink, paper and green.

    python3 scripts/email_templates.py preview [analysis-folder]   # writes HTML previews to email-preview/
    python3 scripts/email_templates.py worker                      # regenerate the Worker's Tidy Tuesday templates

Subscriptions run on the Worker in the CanvassAndAnalyze repo (one Worker, one
subscriber database, a separate list per site). The Worker sends the confirm and
welcome emails, so `worker` writes them into
../CanvassAndAnalyze/subscribe/src/templates-tidytuesday.js; redeploy the Worker after.

Emails:
  confirm  - double opt-in: "Confirm your subscription"
  welcome  - sent once after confirming: where to start
  new_week - one per new analysis: chart, takeaway, a button to the analysis
Each has an HTML and a plain-text version.
"""
import datetime, html, json, os, re, sys

SITE = "https://arunkoundinya.github.io/TidyTuesday/"
SENDER = "canvassandanalyze@gmail.com"     # one sender for all of Arun's sites
WORKER_TEMPLATES = os.path.join(os.path.dirname(__file__), "..", "..", "CanvassAndAnalyze",
                                "subscribe", "src", "templates-tidytuesday.js")

# ---- Brand rules -------------------------------------------------------------
BRAND = {
    "name": "Tidy Tuesday",
    "from_name": "Arun Koundinya Parasa · Tidy Tuesday",
    "tagline": "One dataset a week, turned into a chart.",
    "logo": SITE + "images/logo-mark.png",
    # colours mirror theme.scss
    "paper": "#FBF8F2", "card": "#FFFFFF", "ink": "#0F1B2D", "ink_soft": "#34425A",
    "muted": "#637089", "rule": "#E2DBCD", "accent": "#0B7A5C", "accent_dark": "#075C45",
    # web fonts are unreliable in email clients; these fall back gracefully
    "serif": "Georgia, 'Times New Roman', serif",
    "sans": "-apple-system, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif",
    "mono": "'SFMono-Regular', Menlo, Consolas, monospace",
}
# Subject lines: new week = "New chart: <title>". Voice: first person, plain, short.


def esc(t):
    return html.escape(str(t or ""), quote=True)


def layout(preheader, body_html, unsubscribe_url=None, reason=None):
    b = BRAND
    reason = reason or f"You are receiving this because you subscribed to {b['name']}."
    unsub = (f' · <a href="{esc(unsubscribe_url)}" style="color:{b["muted"]};text-decoration:underline;">Unsubscribe</a>'
             if unsubscribe_url else "")
    return f"""<!doctype html>
<html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<meta name="color-scheme" content="light"><meta name="supported-color-schemes" content="light">
<title>{esc(b['name'])}</title></head>
<body style="margin:0;padding:0;background:{b['paper']};">
<div style="display:none;max-height:0;overflow:hidden;opacity:0;color:transparent;">{esc(preheader)}</div>
<table role="presentation" width="100%" cellpadding="0" cellspacing="0" border="0" style="background:{b['paper']};">
<tr><td align="center" style="padding:28px 14px;">
  <table role="presentation" width="600" cellpadding="0" cellspacing="0" border="0" style="width:100%;max-width:600px;">
    <tr><td style="padding:0 6px 18px;">
      <a href="{SITE}" style="text-decoration:none;color:{b['ink']};">
        <img src="{b['logo']}" width="36" height="36" alt="" style="vertical-align:middle;border:0;border-radius:9px;">
        <span style="font-family:{b['sans']};font-size:19px;font-weight:bold;vertical-align:middle;margin-left:8px;">{esc(b['name'])}</span>
      </a>
    </td></tr>
    <tr><td style="background:{b['card']};border:1px solid {b['rule']};border-radius:14px;padding:34px 34px 30px;">
      {body_html}
    </td></tr>
    <tr><td style="padding:22px 10px 0;font-family:{b['sans']};font-size:12px;line-height:1.6;color:{b['muted']};text-align:center;">
      <em style="font-family:{b['serif']};font-size:13px;">{esc(b['tagline'])}</em><br>
      Charts by Arun Koundinya Parasa ·
      <a href="{SITE}" style="color:{b['muted']};text-decoration:underline;">Visit the site</a><br>
      {esc(reason)}{unsub}
    </td></tr>
  </table>
</td></tr></table>
</body></html>"""


def kicker(text):
    b = BRAND
    return (f'<p style="margin:0 0 10px;font-family:{b["mono"]};font-size:11px;font-weight:bold;'
            f'letter-spacing:1.6px;text-transform:uppercase;color:{b["accent"]};">{esc(text)}</p>')


def heading(text, size=28):
    b = BRAND
    return (f'<h1 style="margin:0 0 14px;font-family:{b["serif"]};font-size:{size}px;line-height:1.2;'
            f'font-weight:normal;color:{b["ink"]};">{esc(text)}</h1>')


def para(text, italic=False, size=16):
    b = BRAND
    style = "font-style:italic;" if italic else ""
    return (f'<p style="margin:0 0 16px;font-family:{b["sans"]};font-size:{size}px;line-height:1.6;'
            f'color:{b["ink_soft"]};{style}">{text}</p>')


def button(label, url, primary=True):
    b = BRAND
    bg, fg, border = (b["accent"], "#FFFFFF", b["accent"]) if primary else ("#FFFFFF", b["ink"], b["rule"])
    return (f'<a href="{esc(url)}" style="display:inline-block;margin:4px 8px 4px 0;padding:12px 22px;'
            f'border-radius:999px;background:{bg};border:1px solid {border};color:{fg};'
            f'font-family:{b["sans"]};font-size:14px;font-weight:bold;text-decoration:none;">{esc(label)}</a>')


def meta_line(text):
    b = BRAND
    return f'<p style="margin:14px 0 0;font-family:{b["sans"]};font-size:12px;color:{b["muted"]};">{esc(text)}</p>'


def sender_note():
    b = BRAND
    return (f'<div style="margin:22px 0 4px;padding:14px 16px;border-radius:10px;background:{b["paper"]};border:1px solid {b["rule"]};'
            f'font-family:{b["sans"]};font-size:14px;line-height:1.55;color:{b["ink_soft"]};">'
            f'<strong>One small request:</strong> my emails come from <strong>{esc(SENDER)}</strong>, the address I use for '
            f'all my sites. Please add it to your contacts so they reach your inbox. If this one landed in Spam or '
            f'Promotions, moving it to your inbox helps too.</div>')


# ---- Emails --------------------------------------------------------------------
def confirm(confirm_url):
    subject = f"Confirm your subscription to {BRAND['name']}"
    pre = "One click and you're in. If this wasn't you, ignore this email."
    body = (kicker("One last step") + heading("Please confirm your subscription")
            + para("Thank you for following along. Click below and you'll get an email whenever I publish a new "
                   "TidyTuesday chart, with the takeaway and a link to the full analysis.")
            + f'<p style="margin:22px 0 6px;">{button("Yes, subscribe me", confirm_url)}</p>'
            + para("If you didn't ask for this, simply ignore this email and you won't hear from me again.", size=14))
    text = (f"Please confirm your subscription to {BRAND['name']}.\n\n"
            f"Confirm here: {confirm_url}\n\nIf you didn't ask for this, ignore this email.\n\n— Arun\n{SITE}")
    return subject, layout(pre, body, reason="You received this because someone entered this address on the Tidy Tuesday site."), text


def welcome(unsubscribe_url):
    subject = f"Welcome to {BRAND['name']}"
    pre = "Here's where to start, and what to expect."
    b = BRAND
    starts = [
        ("The gallery", "Every chart so far, in R and Python. Filter by chart type or year.", SITE + "gallery.html"),
        ("All weeks at a glance", "One table with every dataset, chart type and tool.", SITE + "gallery.html#all-weeks"),
        ("About the project", "Why I do TidyTuesday and how each analysis is built.", SITE + "about.html"),
    ]
    items = "".join(
        f'<tr><td style="padding:12px 0;border-top:1px solid {b["rule"]};">'
        f'<a href="{esc(u)}" style="font-family:{b["sans"]};font-size:16px;font-weight:bold;color:{b["ink"]};text-decoration:none;">{esc(t)} →</a>'
        f'<div style="font-family:{b["sans"]};font-size:14px;line-height:1.5;color:{b["muted"]};margin-top:3px;">{esc(d)}</div></td></tr>'
        for t, d, u in starts)
    body = (kicker("You're subscribed") + heading("Welcome, and thank you")
            + para("TidyTuesday is a weekly data project from the R for Data Science community. I take part when I can, "
                   "in R or Python, and publish each chart with its takeaway. You'll get one email per new chart, and nothing else.")
            + f'<p style="margin:18px 0 6px;font-family:{b["mono"]};font-size:11px;font-weight:bold;letter-spacing:1.4px;text-transform:uppercase;color:{b["muted"]};">Good places to start</p>'
            + f'<table role="presentation" width="100%" cellpadding="0" cellspacing="0" border="0">{items}</table>'
            + sender_note()
            + para("<br>— Arun", size=15))
    text = (f"Welcome to {BRAND['name']}, and thank you.\n\n"
            "You'll get one email per new TidyTuesday chart, and nothing else.\n\nGood places to start:\n"
            + "".join(f"- {t}: {u}\n" for t, _, u in starts)
            + f"\nOne small request: my emails come from {SENDER}, the address I use for all my sites. "
            + "Please add it to your contacts so they reach your inbox.\n\n"
            + f"— Arun\n\nUnsubscribe: {unsubscribe_url}")
    return subject, layout(pre, body, unsubscribe_url), text


def new_week(post, unsubscribe_url):
    """post: dict from post_info()."""
    b = BRAND
    subject = f"New chart: {post['title']}"
    img = (f'<a href="{esc(post["url"])}"><img src="{esc(post["image"])}" width="530" alt="{esc(post["title"])}" '
           f'style="display:block;width:100%;max-width:530px;height:auto;border:1px solid {b["rule"]};border-radius:10px;margin:0 auto 22px;"></a>'
           if post.get("image") else "")
    meta = " · ".join(x for x in [post.get("tools"), post.get("charts")] if x)
    body = (img + kicker(post["kicker"]) + heading(post["title"])
            + (para(esc(post["takeaway"]), italic=True, size=17) if post.get("takeaway") else "")
            + (para(esc(post["description"])) if post.get("description") else "")
            + f'<p style="margin:22px 0 0;">{button("See the analysis", post["url"])}'
            + (button("Dataset", post["dataset"], primary=False) if post.get("dataset") else "") + "</p>"
            + (meta_line(meta) if meta else ""))
    text = (f"{post['kicker']}\n\n{post['title']}\n"
            + (f"{post['takeaway']}\n" if post.get("takeaway") else "")
            + (f"\n{post['description']}\n" if post.get("description") else "")
            + f"\nSee the analysis: {post['url']}\n"
            + (f"Dataset: {post['dataset']}\n" if post.get("dataset") else "")
            + f"\n— Arun\n\nUnsubscribe: {unsubscribe_url}")
    return subject, layout(post.get("takeaway") or post.get("description") or post["title"], body, unsubscribe_url), text


# ---- Analysis data from the site sources ---------------------------------------
def front_matter(path):
    s = open(path, encoding="utf-8").read()
    m = re.match(r"---\n(.*?)\n---\n", s, re.S)
    fm = {}
    for line in (m.group(1).splitlines() if m else []):
        kv = re.match(r"^([A-Za-z-]+):\s*(.*)$", line)
        if kv:
            v = kv.group(2).strip()
            fm[kv.group(1)] = ([x.strip().strip("\"'") for x in v[1:-1].split(",") if x.strip()]
                               if v.startswith("[") and v.endswith("]") else v.strip("\"'"))
    return fm


def source(folder):
    """The analysis's .qmd: index.qmd, or the only .qmd in the folder (the dashboard)."""
    d = os.path.join("tidy-analysis", folder)
    if os.path.exists(os.path.join(d, "index.qmd")):
        return os.path.join(d, "index.qmd")
    qmds = sorted(f for f in os.listdir(d) if f.endswith(".qmd") and not f.startswith("_"))
    return os.path.join(d, qmds[0]) if qmds else None


def analyses():
    """Every analysis folder that has a .qmd."""
    return sorted(f for f in os.listdir("tidy-analysis")
                  if os.path.isdir(os.path.join("tidy-analysis", f)) and source(f))


def post_info(folder):
    fm = front_matter(source(folder))
    url = f"{SITE}tidy-analysis/{folder}/"
    image = fm.get("image", "")
    info = {"slug": folder, "title": fm.get("title", folder), "url": url,
            "takeaway": fm.get("takeaway") or fm.get("subtitle", ""),
            "description": fm.get("description", ""),
            "image": url + image if image and not image.startswith("http") else image,
            "dataset": fm.get("dataset", "") if str(fm.get("dataset", "")).startswith("http") else "",
            "tools": fm.get("tools", ""), "charts": fm.get("charts", ""), "date_iso": fm.get("date", "")}
    week = fm.get("tt-week", "")
    try:
        info["kicker"] = "New chart · Week of " + datetime.date.fromisoformat(week).strftime("%-d %b %Y")
    except ValueError:
        info["kicker"] = "New chart"
    return info


def write_worker_templates(path=WORKER_TEMPLATES):
    """The Worker sends the confirm and welcome emails, so give it the same templates (generated, not hand-copied)."""
    c_subject, c_html, c_text = confirm("{{CONFIRM_URL}}")
    w_subject, w_html, w_text = welcome("{{UNSUBSCRIBE_URL}}")
    js = ("// GENERATED by `python3 scripts/email_templates.py worker` in the TidyTuesday repo. Do not edit.\n"
          f"const C = {json.dumps({'subject': c_subject, 'html': c_html, 'text': c_text}, ensure_ascii=False)};\n"
          f"const W = {json.dumps({'subject': w_subject, 'html': w_html, 'text': w_text}, ensure_ascii=False)};\n"
          "const fill = (t, key, value) => ({ subject: t.subject, html: t.html.split(key).join(value), text: t.text.split(key).join(value) });\n"
          "export const confirmEmail = (url) => fill(C, '{{CONFIRM_URL}}', url);\n"
          "export const welcomeEmail = (url) => fill(W, '{{UNSUBSCRIBE_URL}}', url);\n")
    open(path, "w", encoding="utf-8").write(js)
    print(f"wrote {os.path.normpath(path)}")


if __name__ == "__main__" and len(sys.argv) > 1 and sys.argv[1] == "worker":
    write_worker_templates()

if __name__ == "__main__" and len(sys.argv) > 1 and sys.argv[1] == "preview":
    out = os.environ.get("PREVIEW_DIR", "email-preview")
    if os.environ.get("PREVIEW_LOGO"):                     # logo isn't published yet; show the local copy
        BRAND["logo"] = os.environ["PREVIEW_LOGO"]
    os.makedirs(out, exist_ok=True)
    unsub = SITE + "unsubscribe.html?token=EXAMPLE"
    folders = sys.argv[2:] or [max(analyses(), key=lambda f: post_info(f)["date_iso"])]
    emails = {"1-confirm": confirm(SITE + "confirm.html?token=EXAMPLE"), "2-welcome": welcome(unsub)}
    for i, f in enumerate(folders, 3):
        emails[f"{i}-new-week-{f}"] = new_week(post_info(f), unsub)
    for name, (subject, html_body, text) in emails.items():
        open(os.path.join(out, name + ".html"), "w", encoding="utf-8").write(html_body)
        open(os.path.join(out, name + ".txt"), "w", encoding="utf-8").write(f"Subject: {subject}\n\n{text}")
        print(f"{name}: {subject}")
