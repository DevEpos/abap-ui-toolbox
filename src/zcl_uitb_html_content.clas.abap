CLASS zcl_uitb_html_content DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Adds given text</p>
    "!
    METHODS add_text
      IMPORTING
        iv_text        TYPE w3_html
      RETURNING
        VALUE(rr_self) TYPE REF TO zcl_uitb_html_content.
    "! <p class="shorttext synchronized" lang="en">Start new paragraph</p>
    "!
    METHODS start_p
      RETURNING
        VALUE(rr_self) TYPE REF TO zcl_uitb_html_content.
    "! <p class="shorttext synchronized" lang="en">End paragraph</p>
    "!
    METHODS end_p
      RETURNING
        VALUE(rr_self) TYPE REF TO zcl_uitb_html_content.
    "! <p class="shorttext synchronized" lang="en">Start Blockquote</p>
    "!
    METHODS start_quote
      RETURNING
        VALUE(rr_self) TYPE REF TO zcl_uitb_html_content.
    "! <p class="shorttext synchronized" lang="en">End blockquote</p>
    "!
    METHODS end_quote
      RETURNING
        VALUE(rr_self) TYPE REF TO zcl_uitb_html_content.
    "! <p class="shorttext synchronized" lang="en">Add Heading</p>
    "!
    METHODS add_heading
      IMPORTING
        iv_text        TYPE w3_html
        iv_level       TYPE i DEFAULT 3
      RETURNING
        VALUE(rr_self) TYPE REF TO zcl_uitb_html_content.
    "! <p class="shorttext synchronized" lang="en">Add some bold text</p>
    METHODS add_bold
      IMPORTING
        iv_bold        TYPE w3_html
      RETURNING
        VALUE(rr_self) TYPE REF TO zcl_uitb_html_content.
    "! <p class="shorttext synchronized" lang="en">Add some underlined text</p>
    "!
    METHODS add_underlined
      IMPORTING
        iv_underlined  TYPE w3_html
      RETURNING
        VALUE(rr_self) TYPE REF TO zcl_uitb_html_content.
    "! <p class="shorttext synchronized" lang="en">Add emphasized text</p>
    "!
    METHODS add_emphasized
      IMPORTING
        iv_emphasized  TYPE w3_html
      RETURNING
        VALUE(rr_self) TYPE REF TO zcl_uitb_html_content.
    "! <p class="shorttext synchronized" lang="en">Start new unordered list</p>
    "!
    METHODS start_ul
      RETURNING
        VALUE(rr_self) TYPE REF TO zcl_uitb_html_content.
    "! <p class="shorttext synchronized" lang="en">End unordered list</p>
    "!
    METHODS end_ul
      RETURNING
        VALUE(rr_self) TYPE REF TO zcl_uitb_html_content.
    "! <p class="shorttext synchronized" lang="en">Start new ordered list</p>
    "!
    METHODS start_ol
      RETURNING
        VALUE(rr_self) TYPE REF TO zcl_uitb_html_content.
    "! <p class="shorttext synchronized" lang="en">End ordered list</p>
    "!
    METHODS end_ol
      RETURNING
        VALUE(rr_self) TYPE REF TO zcl_uitb_html_content.
    "! <p class="shorttext synchronized" lang="en">Start new list item element</p>
    "!
    METHODS start_li
      RETURNING
        VALUE(rr_self) TYPE REF TO zcl_uitb_html_content.
    "! <p class="shorttext synchronized" lang="en">Add new list item element</p>
    "!
    METHODS add_li
      IMPORTING
        iv_list_item_text TYPE w3_html
      RETURNING
        VALUE(rr_self)    TYPE REF TO zcl_uitb_html_content.
    "! <p class="shorttext synchronized" lang="en">End list item element</p>
    "!
    METHODS end_li
      RETURNING
        VALUE(rr_self) TYPE REF TO zcl_uitb_html_content.
    "! <p class="shorttext synchronized" lang="en">Start new html table</p>
    "!
    METHODS start_table
      RETURNING
        VALUE(rr_self) TYPE REF TO zcl_uitb_html_content.
    "! <p class="shorttext synchronized" lang="en">Closes HTML table</p>
    "!
    METHODS end_table
      RETURNING
        VALUE(rr_self) TYPE REF TO zcl_uitb_html_content.
    "! <p class="shorttext synchronized" lang="en">Starts new table row</p>
    METHODS start_table_row
      RETURNING
        VALUE(rr_self) TYPE REF TO zcl_uitb_html_content.
    "! <p class="shorttext synchronized" lang="en">Ends current table row</p>
    "!
    METHODS end_table_row
      RETURNING
        VALUE(rr_self) TYPE REF TO zcl_uitb_html_content.
    "! <p class="shorttext synchronized" lang="en">Adds new table header cell</p>
    "!
    METHODS add_table_header_cell
      IMPORTING
        iv_text        TYPE w3_html
      RETURNING
        VALUE(rr_self) TYPE REF TO zcl_uitb_html_content.
    "! <p class="shorttext synchronized" lang="en">Adds new table body cell</p>
    "!
    METHODS add_table_body_cell
      IMPORTING
        iv_text        TYPE w3_html
      RETURNING
        VALUE(rr_self) TYPE REF TO zcl_uitb_html_content.
    "! <p class="shorttext synchronized" lang="en">Retrieve complete html content</p>
    "!
    METHODS get_content
      RETURNING
        VALUE(rt_content) TYPE w3htmltab.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mt_content TYPE w3htmltab.

    "! <p class="shorttext synchronized" lang="en">Add text with tag</p>
    "!
    METHODS add_with_tag
      IMPORTING
        iv_text TYPE string
        iv_tag  TYPE string .
    "! <p class="shorttext synchronized" lang="en">Writes a start tag</p>
    "!
    METHODS write_start_tag
      IMPORTING
        iv_tag TYPE string .
    "! <p class="shorttext synchronized" lang="en">Writes a closing tag</p>
    METHODS write_end_tag
      IMPORTING
        iv_tag TYPE string .
ENDCLASS.



CLASS zcl_uitb_html_content IMPLEMENTATION.
  METHOD add_bold.
    add_with_tag( iv_text = |{ iv_bold }|  iv_tag  = 'strong' ).
    rr_self = me.
  ENDMETHOD.

  METHOD add_emphasized.
    add_with_tag( iv_text = |{ iv_emphasized }|  iv_tag  = 'em' ).
    rr_self = me.
  ENDMETHOD.

  METHOD add_heading.
    DATA(lv_level) = COND #( WHEN iv_level > 1 AND iv_level < 7 THEN iv_level ELSE 3 ).
    add_with_tag( iv_text = |{ iv_text }|  iv_tag  = |h{ lv_level }| ).
    rr_self = me.
  ENDMETHOD.

  METHOD add_li.
    add_with_tag( iv_text = |{ iv_list_item_text }|  iv_tag  = 'li' ).
    rr_self = me.
  ENDMETHOD.

  METHOD add_underlined.
    add_with_tag( iv_text = |{ iv_underlined }|  iv_tag  = 'u' ).
    rr_self = me.
  ENDMETHOD.

  METHOD end_li.
    write_end_tag( 'li' ).
    rr_self = me.
  ENDMETHOD.

  METHOD end_ol.
    write_end_tag( 'ol' ).
    rr_self = me.
  ENDMETHOD.

  METHOD end_p.
    write_end_tag( 'p' ).
    rr_self = me.
  ENDMETHOD.

  METHOD end_quote.
    write_end_tag( 'blockquote' ).
    rr_self = me.
  ENDMETHOD.

  METHOD end_ul.
    write_end_tag( 'ul' ).
    rr_self = me.
  ENDMETHOD.

  METHOD start_li.
    write_start_tag( 'li' ).
    rr_self = me.
  ENDMETHOD.

  METHOD start_ol.
    write_start_tag( 'ol' ).
    rr_self = me.
  ENDMETHOD.

  METHOD start_p.
    write_start_tag( 'p' ).
    rr_self = me.
  ENDMETHOD.

  METHOD start_quote.
    write_start_tag( 'blockquote' ).
    rr_self = me.
  ENDMETHOD.

  METHOD start_ul.
    write_start_tag( 'ul' ).
    rr_self = me.
  ENDMETHOD.

  METHOD add_with_tag.
    mt_content = VALUE #(
      BASE mt_content
      ( |<{ iv_tag }>{ iv_text }</{ iv_tag }>| )
    ).
  ENDMETHOD.

  METHOD write_end_tag.
    mt_content = VALUE #(
      BASE mt_content
      ( |</{ iv_tag }>| )
    ).
  ENDMETHOD.

  METHOD write_start_tag.
    mt_content = VALUE #(
      BASE mt_content
      ( |<{ iv_tag }>| )
    ).
  ENDMETHOD.

  METHOD get_content.
    rt_content = mt_content.
  ENDMETHOD.

  METHOD add_text.
    mt_content = VALUE #(
      BASE mt_content
      ( |{ iv_text }| )
    ).
    rr_self = me.
  ENDMETHOD.

  METHOD add_table_body_cell.
    add_with_tag(
        iv_text = |{ iv_text }|
        iv_tag  = 'td'
    ).
    rr_self = me.
  ENDMETHOD.

  METHOD add_table_header_cell.
    add_with_tag(
        iv_text = |{ iv_text }|
        iv_tag  = 'th'
    ).
    rr_self = me.
  ENDMETHOD.

  METHOD end_table.
    write_end_tag( iv_tag = 'table' ).
    rr_self = me.
  ENDMETHOD.

  METHOD start_table.
    write_start_tag( iv_tag = 'table' ).
    rr_self = me.
  ENDMETHOD.

  METHOD end_table_row.
    write_end_tag( iv_tag = 'tr' ).
    rr_self = me.
  ENDMETHOD.

  METHOD start_table_row.
    write_start_tag( iv_tag = 'tr' ).
    rr_self = me.
  ENDMETHOD.

ENDCLASS.
