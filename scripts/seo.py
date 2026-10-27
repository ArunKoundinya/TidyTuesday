"""Quarto post-render step: adds the SEO tags Quarto doesn't emit on its own.

Every rendered page gets a canonical URL. Each analysis also gets author/date
meta tags and schema.org Article JSON-LD (with the dataset it uses); the home page
describes the site. Runs automatically after `quarto render`.
"""
import json, os, re

cfg = open("_quarto.yml", encoding="utf-8").read()
m = re.search(r"^\s*site-url:\s*(\S+)", cfg, re.M)
SITE = (m.group(1) if m else "").rstrip("/") + "/"
OUT = "_site"
NAME = "Tidy Tuesday by Arun Koundinya"
AUTHOR = {"@type": "Person", "name": "Arun Koundinya Parasa", "url": "https://arunkoundinya.github.io/",
          "sameAs": ["https://www.linkedin.com/in/arunkoundinya0710/", "https://github.com/ArunKoundinya"]}
MARK = "<!-- seo.py -->"


def simple_yaml(text):
    """Enough YAML for front matter: `key: value` lines, `[a, b]` lists,
    and a top-level list of such mappings. Keeps this script free of extra packages."""
    def val(v):
        v = v.strip()
        if v.startswith("[") and v.endswith("]"):
            return [x.strip().strip("\"'") for x in v[1:-1].split(",") if x.strip()]
        v = v.strip("\"'")
        return int(v) if v.isdigit() else v
    items, cur = [], {}
    for line in text.splitlines():
        if not line.strip() or line.lstrip().startswith("#"):
            continue
        if line.startswith("- "):
            cur = {}
            items.append(cur)
            line = line[2:]
        kv = re.match(r"^\s*([A-Za-z_-]+):\s*(.*)$", line)
        if kv:
            cur[kv.group(1)] = val(kv.group(2))
    return items if items else cur


def front_matter(src):
    try:
        s = open(src, encoding="utf-8").read()
    except OSError:
        return {}
    m = re.match(r"---\n(.*?)\n---", s, re.S)
    return simple_yaml(m.group(1)) if m else {}


def page_url(rel):
    rel = rel.replace(os.sep, "/")
    return SITE + (rel[: -len("index.html")] if rel.endswith("index.html") else rel)


def source_for(rel):
    """The .qmd a page came from. Analyses render to index.html whatever the source is called."""
    src = os.path.splitext(rel)[0] + ".qmd"
    if os.path.exists(src) or not rel.startswith("tidy-analysis/"):
        return src
    folder = os.path.dirname(rel)
    qmds = [f for f in os.listdir(folder) if f.endswith(".qmd")] if os.path.isdir(folder) else []
    return os.path.join(folder, qmds[0]) if qmds else src


def tags_for(rel):
    url = page_url(rel)
    out = [f'<link rel="canonical" href="{url}">']
    fm = front_matter(source_for(rel))
    is_post = rel.startswith("tidy-analysis/")

    if is_post and fm:
        image = fm.get("image")
        data = {
            "@context": "https://schema.org",
            "@type": "Article",
            "headline": str(fm.get("title", "")).strip(),
            "description": fm.get("description", ""),
            "datePublished": str(fm.get("date", "")),
            "author": AUTHOR,
            "publisher": AUTHOR,
            "mainEntityOfPage": url,
            "url": url,
            "inLanguage": "en",
            "isPartOf": {"@type": "WebSite", "name": NAME, "url": SITE},
        }
        if image:
            data["image"] = image if image.startswith("http") else url + image
        if isinstance(fm.get("categories"), list):
            data["keywords"] = ", ".join(["TidyTuesday"] + fm["categories"])
        if str(fm.get("dataset", "")).startswith("http"):
            data["about"] = {"@type": "Dataset", "name": f'TidyTuesday {fm.get("tt-week", "")}'.strip(),
                             "url": fm["dataset"]}
        out += [
            '<meta name="author" content="Arun Koundinya Parasa">',
            '<meta property="og:type" content="article">',
            f'<meta property="article:published_time" content="{fm.get("date", "")}">',
            f'<meta property="og:url" content="{url}">',
        ]
    elif rel == "index.html":
        data = {
            "@context": "https://schema.org",
            "@type": "WebSite",
            "name": NAME,
            "url": SITE,
            "description": fm.get("description", ""),
            "author": AUTHOR,
            "potentialAction": {"@type": "SearchAction",
                                "target": SITE + "search.html?q={query}",
                                "query-input": "required name=query"},
        }
    else:
        data = None

    if not is_post:
        out += ['<meta property="og:type" content="website">', f'<meta property="og:url" content="{url}">']
    if data:
        out.append('<script type="application/ld+json">' + json.dumps(data, ensure_ascii=False) + "</script>")
    out.append('<link rel="apple-touch-icon" href="' + SITE + 'images/apple-touch-icon.png">')
    return MARK + "\n" + "\n".join(out) + "\n"


files = os.environ.get("QUARTO_PROJECT_OUTPUT_FILES", "").split("\n")
files = [f for f in files if f.endswith(".html")] or [
    os.path.join(d, f) for d, _, fs in os.walk(OUT) for f in fs if f.endswith(".html")]

done = 0
for path in files:
    rel = os.path.relpath(path, OUT).replace(os.sep, "/")
    if rel.startswith("site_libs"):
        continue
    html = open(path, encoding="utf-8").read()
    if MARK in html or "</head>" not in html:
        continue
    html = re.sub(r'\s*<meta property="og:(type|url)"[^>]*>', "", html)
    # WhatsApp and LinkedIn previews are pickier than Google: give the image's type and alt text.
    m = re.search(r'<meta property="og:image" content="([^"]+)">', html)
    if m:
        ext = m.group(1).rsplit(".", 1)[-1].lower()
        kind = {"jpg": "jpeg", "jpeg": "jpeg", "png": "png", "webp": "webp", "gif": "gif"}.get(ext)
        title = re.search(r'<meta property="og:title" content="([^"]*)"', html)
        extra = (f'\n<meta property="og:image:type" content="image/{kind}">' if kind else "") + \
                (f'\n<meta property="og:image:alt" content="{title.group(1)}">' if title else "")
        html = html.replace(m.group(0), m.group(0) + extra, 1)
    if rel in ("confirm.html", "unsubscribe.html"):              # subscription status pages: keep out of search
        html = html.replace('content="index, follow, max-image-preview:large"', 'content="noindex"', 1)
    html = html.replace("</head>", tags_for(rel) + "</head>", 1)
    open(path, "w", encoding="utf-8").write(html)
    done += 1

# Subscription status pages stay out of the sitemap (Quarto has no per-page switch for it)
sitemap = os.path.join(OUT, "sitemap.xml")
if os.path.exists(sitemap):
    xml = open(sitemap, encoding="utf-8").read()
    trimmed = re.sub(r"\s*<url>\s*<loc>[^<]*/(?:confirm|unsubscribe)\.html</loc>.*?</url>", "", xml, flags=re.S)
    if trimmed != xml:
        open(sitemap, "w", encoding="utf-8").write(trimmed)

print(f"seo.py: tagged {done} pages")
