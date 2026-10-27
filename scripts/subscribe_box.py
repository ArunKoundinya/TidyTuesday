"""Quarto post-render step: wire up the email subscribe forms.

Reads subscribe/site.json (the shared Worker's URL, the public Turnstile site key and
this site's list name). The forms themselves are placeholders in the pages: the home
page band and the end of every analysis (added by _filters/tt-meta.lua).
  - every .subscribe-form placeholder gets data-endpoint / data-sitekey / data-list
  - pages with a form load assets/subscribe.js
  - confirm.html and unsubscribe.html learn the Worker URL
"""
import json, os, re

OUT = "_site"
cfg = json.load(open("subscribe/site.json"))
endpoint, sitekey, name = cfg.get("worker_url", "").rstrip("/"), cfg.get("turnstile_site_key", ""), cfg.get("list", "")

files = os.environ.get("QUARTO_PROJECT_OUTPUT_FILES", "").split("\n")
files = [f for f in files if f.endswith(".html")] or [
    os.path.join(d, f) for d, _, fs in os.walk(OUT) for f in fs if f.endswith(".html")]

changed = 0
if endpoint:
    for path in files:
        rel = os.path.relpath(path, OUT).replace(os.sep, "/")
        if rel.startswith("site_libs") or not os.path.exists(path):
            continue
        page = open(path, encoding="utf-8").read()
        if "data-endpoint=" in page:                   # already wired
            continue
        root = "../" * rel.count("/")
        new = page
        if 'class="subscribe-form"' in new:
            new = new.replace('class="subscribe-form"',
                              f'class="subscribe-form" data-endpoint="{endpoint}" data-sitekey="{sitekey}" data-list="{name}"')
            new = new.replace("</body>", f'<script src="{root}assets/subscribe.js" defer></script>\n</body>', 1)
        if rel in ("unsubscribe.html", "confirm.html") and "data-subscribe-endpoint" not in new:
            new = re.sub(r"<body([^>]*)>", lambda m: f'<body{m.group(1)} data-subscribe-endpoint="{endpoint}">', new, count=1)
        if new != page:
            open(path, "w", encoding="utf-8").write(new)
            changed += 1
print(f"subscribe_box.py: {'wired ' + str(changed) + ' pages' if endpoint else 'skipped (subscribe/site.json has no worker_url)'}")
