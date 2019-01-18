"! <p class="shorttext synchronized" lang="en">Help Dialog Box</p>
CLASS zcl_uitb_popup_help_viewer DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_uitb_view.
    ALIASES show
      FOR zif_uitb_view~show.
    "! <p class="shorttext synchronized" lang="en">Creates new help viewer</p>
    "!
    CLASS-METHODS create
      IMPORTING
        iv_title        TYPE string OPTIONAL
        iv_style        TYPE string OPTIONAL
        it_doc_lines    TYPE string_table OPTIONAL
        ir_html_content TYPE REF TO zcl_uitb_html_content OPTIONAL
      RETURNING
        VALUE(result)   TYPE REF TO zcl_uitb_popup_help_viewer.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: tt_w3_mime TYPE STANDARD TABLE OF w3_mime WITH DEFAULT KEY.
    DATA mr_dialog_box TYPE REF TO cl_gui_dialogbox_container .
    DATA mt_doc_lines TYPE string_table .
    DATA mr_html_content TYPE REF TO zcl_uitb_html_content.
    "! <p class="shorttext synchronized" lang="en">HTML Control Proxy Class</p>
    DATA mr_html_viewer TYPE REF TO cl_gui_html_viewer .
    "! <p class="shorttext synchronized" lang="en">Table for ABAP Keyword Documentation</p>
    DATA mt_html TYPE w3htmltab .
    "! <p class="shorttext synchronized" lang="en">Text (80 Characters)</p>
    DATA mv_html_url TYPE text80 .
    DATA mv_html_size TYPE i.
    "! <p class="shorttext synchronized" lang="en">ITS: Table with MIMEs</p>
    DATA mt_html_raw TYPE tt_w3_mime.
    DATA mv_title TYPE string.
    DATA mv_style TYPE string.

    METHODS create_document .
    METHODS constructor
      IMPORTING
        iv_title        TYPE string
        iv_style        TYPE string
        it_doc_lines    TYPE string_table
        ir_html_content TYPE REF TO zcl_uitb_html_content.
    METHODS on_close
        FOR EVENT close OF cl_gui_dialogbox_container .
    METHODS build_html .
ENDCLASS.



CLASS zcl_uitb_popup_help_viewer IMPLEMENTATION.


  METHOD build_html.
    DATA: lv_string TYPE string.

    CONCATENATE LINES OF mt_html INTO lv_string SEPARATED BY cl_abap_char_utilities=>newline.

    DATA: lv_xstr  TYPE xstring.


    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = lv_string
      IMPORTING
        buffer = lv_xstr
      EXCEPTIONS
        OTHERS = 1.
    ASSERT sy-subrc = 0.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = lv_xstr
      IMPORTING
        output_length = mv_html_size
      TABLES
        binary_tab    = mt_html_raw.
  ENDMETHOD.


  METHOD constructor.
    mv_title = iv_title.
    mt_doc_lines = it_doc_lines.
    mv_style = iv_style.
    mr_html_content = ir_html_content.
  ENDMETHOD.


  METHOD create.
    ASSERT NOT ( it_doc_lines IS NOT INITIAL AND ir_html_content IS BOUND ).
    ASSERT NOT ( it_doc_lines IS INITIAL AND ir_html_content IS INITIAL ).

    result = NEW #(
      iv_title        = iv_title
      iv_style        = iv_style
      it_doc_lines    = it_doc_lines
      ir_html_content = ir_html_content
    ).
    result->create_document( ).
  ENDMETHOD.


  METHOD create_document.
    mt_html = VALUE #(
      ( line = |<!DOCTYPE html>| )
      ( line = |<html>| )
      ( line = |<head>| )
      ( line = |<meta charset="utf-8">| )
      ( line = |<style type="text/css"> <!--| )
      ( line = |body \{| )
      ( line = |  background-color: #fefeee;| )
      ( line = |  font-family: arial;| )
      ( line = |  font-style: normal;| )
      ( line = |  font-size: 0.8em;| )
      ( line = |  color: #000000;| )
      ( line = |\}| )
      ( line = || )
      ( line = |table, td, tr \{| )
      ( line = |  background-color: #fefeee;| )
      ( line = |  font-family: arial;| )
      ( line = |  font-style: normal;| )
      ( line = |  font-size: 100%;| )
      ( line = |  color: #000000;| )
      ( line = |\}| )
      ( line = || )
      ( line = |h1, h2, h3, h4, h5 \{| )
      ( line = |  color: #000080;| )
      ( line = |\}| )
      ( line = || )
      ( line = |a:link \{| )
      ( line = |  color: #0063a4;| )
      ( line = |  text-decoration: none;| )
      ( line = |\}| )
      ( line = || )
      ( line = |a:visited \{| )
      ( line = |  color: #807f75;| )
      ( line = |  text-decoration: none;| )
      ( line = |\}| )
      ( line = || )
      ( line = |a:active \{| )
      ( line = |  color: #807f75;| )
      ( line = |  text-decoration: none;| )
      ( line = |\}| )
      ( line = || )
      ( line = |a:hover \{| )
      ( line = |  color: #f00000;| )
      ( line = |  text-decoration: none;| )
      ( line = |\}| )
      ( line = || )
      ( line = |.button \{| )
      ( line = |  font-family: arial, sans-serif;| )
      ( line = |\}| )
      ( line = || )
      ( line = |.inactive \{| )
      ( line = |  font-family: arial, sans-serif;| )
      ( line = |  color: #919186;| )
      ( line = |\}| )
      ( line = || )
      ( line = |.text \{| )
      ( line = |  font-family: arial, sans-serif;| )
      ( line = |\}| )
      ( line = || )
      ( line = |.tab \{| )
      ( line = |  font-family: arial, sans-serif;| )
      ( line = |\}| )
      ( line = || )
      ( line = |.groupbox \{| )
      ( line = |  font-family: arial, sans-serif;| )
      ( line = |  color: #42423d;| )
      ( line = |\}| )
      ( line = || )
      ( line = |.tableheader \{| )
      ( line = |  font-family: arial, sans-serif;| )
      ( line = |  color: #42423d;| )
      ( line = |\}| )
      ( line = || )
      ( line = |.nonedit \{| )
      ( line = |  font-family: arial, sans-serif;| )
      ( line = |  color: #42423d;| )
      ( line = |\}| )
      ( line = || )
      ( line = |.input \{| )
      ( line = |  font-family: arial, sans-serif;| )
      ( line = |  background: #fefeee;| )
      ( line = |  border: 0;| )
      ( line = |  padding-left: 5px;| )
      ( line = |  padding-top: 2px;| )
      ( line = |  padding-bottom: 2px;| )
      ( line = |  padding-right: 5px;| )
      ( line = |\}| )
      ( line = || )
      ( line = |.pulldown \{| )
      ( line = |  font-family: arial, sans-serif;| )
      ( line = |  background: #fefeee;| )
      ( line = |  border: 0;| )
      ( line = |\}| )
    ).

    IF mv_style IS NOT INITIAL.
      mt_html = VALUE #( BASE mt_html
        ( line = mv_style )
      ).
    ENDIF.

    mt_html = VALUE #( BASE mt_html
      ( line = || )
      ( line = |--> </style>| )
      ( line = |</head>| )
      ( line = |<body>| )
    ).

    IF mr_html_content IS BOUND.
      mt_html = VALUE #(
        BASE mt_html
        ( LINES OF mr_html_content->get_content( ) )
      ).
    ELSE.
      mt_html = VALUE #(
        BASE mt_html
        ( LINES OF VALUE #( FOR line IN mt_doc_lines ( line = line ) ) )
      ).
    ENDIF.

    mt_html = VALUE #( BASE mt_html
      ( line = |</body>| )
      ( line = |</html>| )
    ).

    build_html( ).
  ENDMETHOD.


  METHOD on_close.
    mr_dialog_box->set_visible( cl_gui_control=>visible_false ).
  ENDMETHOD.


  METHOD zif_uitb_view~show.
    DATA(ls_metrics) = cl_gui_cfw=>get_metric_factors( ).

    data(lv_top) = ( ls_metrics-screen-y - 350 ) / 2.
    data(lv_left) = ( ls_metrics-screen-x - 900 ) / 2.

    mr_dialog_box = NEW cl_gui_dialogbox_container(
        width   = 900
        height  = 350
        top     = 120
        left    = 300
*        top     = lv_top
*        left    = lv_left
        caption = |{ COND #( WHEN mv_title IS NOT INITIAL THEN mv_title ELSE 'Help'(001) ) }|
    ).

    SET HANDLER: on_close FOR mr_dialog_box.

    mr_html_viewer = NEW #( parent = mr_dialog_box query_table_disabled = abap_true ).

    " load html code into control
    mr_html_viewer->load_data(
      EXPORTING
        size                   = mv_html_size    " Length of Data
      IMPORTING
        assigned_url           = mv_html_url    " URL
      CHANGING
        data_table             = mt_html_raw    " data table
    ).
    IF sy-subrc <> 0.
    ENDIF.

    mr_html_viewer->show_data(
      EXPORTING
        url                    = mv_html_url    " URL
    ).
    IF sy-subrc <> 0.
    ENDIF.

    mr_dialog_box->set_visible( cl_gui_control=>visible_true ).
  ENDMETHOD.
ENDCLASS.
