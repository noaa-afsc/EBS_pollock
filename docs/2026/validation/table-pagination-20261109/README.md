# PDF table pagination verification

Baseline: c79dc3f. Rendered the 54 tables in the main Tables section from canonical HTML using render_main_tables_paginated.py, then integrated them using apply_main_table_pagination.py. Each starts on a fresh page with caption and note spacing. Forty-eight tables fit one page; Tables 1, 2, 5, 28, 32 and 34 span two pages with repeated headers. Inline Table 55 and appendix layouts remain as supplied.

All table text and values match HTML, including repeated headers. Other report body text and figure pixels are preserved; page numbers, bookmarks and internal links were remapped. External source links remain present. Computer Modern Unicode fonts are embedded. No model inputs or results changed. Manual accessibility testing was not performed.
