-- Render editable CSV tables and figure divisions as native Pandoc objects.
-- Formatting lives here and in CSS; report prose and captions stay in QMD.
local function read_file(path)
  local file = assert(io.open(path, 'rb'), 'Cannot open '..path)
  local value = file:read('*a'); file:close(); return value
end

local function csv_rows(text)
  local rows, row, field = {}, {}, ''
  local quoted, i = false, 1
  while i <= #text do
    local c = text:sub(i,i)
    if c == '"' then
      if quoted and text:sub(i+1,i+1) == '"' then field=field..'"'; i=i+1
      else quoted=not quoted end
    elseif c == ',' and not quoted then
      row[#row+1]=field;field=''
    elseif (c == '\n' or c == '\r') and not quoted then
      row[#row+1]=field;field='';rows[#rows+1]=row;row={}
      if c=='\r' and text:sub(i+1,i+1)=='\n' then i=i+1 end
    else field=field..c end
    i=i+1
  end
  assert(not quoted, 'Unclosed CSV quote')
  if #field>0 or #row>0 then row[#row+1]=field;rows[#rows+1]=row end
  table.remove(rows,1) -- CSV editing labels (cell_1, cell_2, ...).
  return rows
end

local align = {left=pandoc.AlignLeft, right=pandoc.AlignRight, center=pandoc.AlignCenter}
local function blocks(text)
  return pandoc.read(text, 'markdown').blocks
end
local function anchor(id)
  return pandoc.Plain{pandoc.Span({},pandoc.Attr(id))}
end

function Div(div)
  if div.classes:includes('report-table') then
    local path=assert(div.attributes['data-table'],'Table CSV path missing')
    local spec=pandoc.json.decode(read_file(path:gsub('%.csv$','.json')))
    local data=csv_rows(read_file(path))
    local head, body, foot = {}, {}, {}
    for r,values in ipairs(data) do
      local cells={}
      for c,value in ipairs(values) do
        local key=r..':'..c
        local span=spec.spans[key] or {1,1}
        local classes={}
        for name in (spec.classes[key] or ''):gmatch('%S+') do classes[#classes+1]=name end
        cells[#cells+1]=pandoc.Cell(blocks(value),align[spec.align[key] or spec.column_align[c]],
                                   span[1],span[2],pandoc.Attr('',classes))
      end
      local rows = r<=spec.header_rows and head or r>#data-spec.footer_rows and foot or body
      rows[#rows+1]=pandoc.Row(cells)
    end
    local colspecs={}
    for c=1,spec.columns do colspecs[c]={align[spec.column_align[c]],nil} end
    local caption=div.content
    local result=pandoc.Table({long=caption,short={}},colspecs,pandoc.TableHead(head),
      {{attr=pandoc.Attr(),body=body,head={},row_head_columns=0}},pandoc.TableFoot(foot),
      pandoc.Attr('',{'assessment-data-table'}))
    return pandoc.Div({anchor(div.identifier),result},pandoc.Attr('',{'report-table'}, {['data-table']=path}))
  end
  if div.classes:includes('report-figure') then
    div.attributes['data-public-id']=div.identifier
    div.identifier='public-'..div.identifier
    return div
  end
end
