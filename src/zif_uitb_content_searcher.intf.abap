"! <p class="shorttext synchronized" lang="en">Content Searcher</p>
INTERFACE zif_uitb_content_searcher
  PUBLIC .

  "! <p class="shorttext synchronized" lang="en">Start searching</p>
  "!
  METHODS search.
  "! <p class="shorttext synchronized" lang="en">Continue searching</p>
  "!
  "! @parameter iv_search_value | <p class="shorttext synchronized" lang="en"></p>
  METHODS search_next
    IMPORTING
      iv_search_value TYPE string OPTIONAL.
ENDINTERFACE.
