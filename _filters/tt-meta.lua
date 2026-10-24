-- Adds a fact strip under the title of each analysis (TidyTuesday week, dataset,
-- tools, chart types) and a "back to the gallery" block at the end.
-- Reads these front-matter fields, all optional:
--   tt-week: "2024-01-16"        # TidyTuesday release the data comes from
--   dataset: <url>               # where the data lives
--   dataset-name: "..."          # link text for the dataset (default: "TidyTuesday <week>")
--   tools: "Python · plotnine"   # language and main packages
--   charts: "Line chart"         # chart types used

local function str(v)
  if v == nil then return nil end
  local s = pandoc.utils.stringify(v)
  if s == "" then return nil end
  return s
end

local function esc(s)
  return (s:gsub("&", "&amp;"):gsub("<", "&lt;"):gsub(">", "&gt;"):gsub('"', "&quot;"))
end

local function pretty_date(iso)
  local y, m, d = iso:match("^(%d%d%d%d)-(%d%d)-(%d%d)$")
  if not y then return iso end
  local months = { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" }
  return string.format("%d %s %s", tonumber(d), months[tonumber(m)], y)
end

function Pandoc(doc)
  if not quarto.doc.is_format("html") or quarto.doc.is_format("dashboard") then
    return doc
  end
  local m = doc.meta
  local week, dataset, tools, charts = str(m["tt-week"]), str(m["dataset"]), str(m["tools"]), str(m["charts"])
  if not (week or dataset or tools or charts) then return doc end

  local cells = {}
  local function cell(k, v) table.insert(cells, '<div><span class="k">' .. k .. '</span><span class="v">' .. v .. '</span></div>') end
  cell("TidyTuesday week", week and esc(pretty_date(week)) or "Bonus dataset")
  if dataset then
    local name = str(m["dataset-name"]) or "Data on GitHub"
    cell("Dataset", '<a href="' .. esc(dataset) .. '">' .. esc(name) .. ' ↗</a>')
  end
  if tools then cell("Built with", esc(tools)) end
  if charts then cell("Charts", esc(charts)) end

  table.insert(doc.blocks, 1, pandoc.RawBlock("html", '<div class="tt-facts">' .. table.concat(cells) .. '</div>'))
  table.insert(doc.blocks, pandoc.RawBlock("html",
    '<div class="tt-next"><p><strong>One week down.</strong> See the other charts in the gallery, ' ..
    'or leave a comment below.</p><a class="btn-ak" href="../../index.html">← Back to the gallery</a></div>'))
  return doc
end
