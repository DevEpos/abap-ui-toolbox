CLASS zcl_uitb_abap_code_viewer DEFINITION
  PUBLIC
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_uitb_view.
    ALIASES show
      FOR zif_uitb_view~show.

    CLASS-METHODS show_code
      IMPORTING
        iv_title TYPE string
        it_code  TYPE string_table OPTIONAL
        iv_code  TYPE string OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS c_prism_css_mime_path TYPE string VALUE '/SAP/PUBLIC/ZUITB/prism.css' ##no_text.
    CONSTANTS c_prism_js_mime_path TYPE string VALUE '/SAP/PUBLIC/ZUITB/prism.js' ##no_text.

    DATA mv_prism_js TYPE string.
    DATA mv_prism_css TYPE string.
    DATA mt_code TYPE string_table.
    DATA mt_output TYPE string_table.

    TYPES: tt_w3_mime TYPE STANDARD TABLE OF w3_mime WITH DEFAULT KEY.
    DATA mr_dialog_box TYPE REF TO cl_gui_dialogbox_container .
    "! <p class="shorttext synchronized" lang="en">HTML Control Proxy Class</p>
    DATA mr_html_viewer TYPE REF TO cl_gui_html_viewer .
    "! <p class="shorttext synchronized" lang="en">Text (80 Characters)</p>
    DATA mv_html_url TYPE text80 .
    DATA mv_html_size TYPE i.
    "! <p class="shorttext synchronized" lang="en">ITS: Table with MIMEs</p>
    DATA mt_html_raw TYPE tt_w3_mime.
    DATA mv_title TYPE string.

    "! <p class="shorttext synchronized" lang="en">Load mime data for path</p>
    "!
    METHODS load_mime
      IMPORTING
        iv_path                TYPE string
      RETURNING
        VALUE(rv_mime_content) TYPE string.
    "! <p class="shorttext synchronized" lang="en">Load prism library from mime</p>
    "!
    METHODS load_prism.
    "! <p class="shorttext synchronized" lang="en">Create HTML document for displaying the code</p>
    "!
    METHODS create_document.

    "! <p class="shorttext synchronized" lang="en">Close handler for dialog box container</p>
    "!
    METHODS on_close
        FOR EVENT close OF cl_gui_dialogbox_container .
ENDCLASS.



CLASS zcl_uitb_abap_code_viewer IMPLEMENTATION.

  METHOD load_prism.
    mv_prism_css = load_mime( c_prism_css_mime_path ).
    mv_prism_js = load_mime( c_prism_js_mime_path ).
  ENDMETHOD.

  METHOD load_mime.
    DATA: mv_mime_content_x_string TYPE xstring.

    cl_mime_repository_api=>get_api( )->get(
     EXPORTING i_url = iv_path
     IMPORTING e_content = mv_mime_content_x_string
     EXCEPTIONS OTHERS = 1 ).

    CHECK sy-subrc = 0.

    DATA(lr_converter) = cl_abap_conv_in_ce=>create(
      encoding = 'UTF-8'
      input    = mv_mime_content_x_string
    ).

    lr_converter->read( IMPORTING data = rv_mime_content ).
  ENDMETHOD.

  METHOD show_code.
    zcl_uitb_screen_util=>show_progress( iv_progress = 1 iv_text = |{ 'Source Code is being prepared...'(002) }| ).

    data(lv_prism_pre_tag_start) = |<pre><code class="language-abap">|.

    DATA(lr_code_viewer) = NEW zcl_uitb_abap_code_viewer( ).
    IF iv_code IS NOT INITIAL.
      lr_code_viewer->mt_code = VALUE #( ( lv_prism_pre_tag_start && iv_code ) ).
    ELSEIF it_code IS NOT INITIAL.
      lr_code_viewer->mt_code = it_code.
      lr_code_viewer->mt_code[ 1 ] = lv_prism_pre_tag_start && lr_code_viewer->mt_code[ 1 ].
    ENDIF.
    lr_code_viewer->mv_title = iv_title.
    lr_code_viewer->load_prism( ).
    lr_code_viewer->create_document( ).
    lr_code_viewer->show( ).
  ENDMETHOD.


  METHOD create_document.
    DATA: lv_string TYPE string,
          lv_xstr   TYPE xstring.

    mt_output = VALUE #(
     ( |<!DOCTYPE html>| )
     ( |<html>| )
     ( |<head>| )
     ( |<meta charset="utf-8">| )
     ( |<style type="text/css"> <!--| )
     ( mv_prism_css )
     ( |--> </style>| )
     ( |</head>| )
     ( |<body class="prism-default" style="background: #f5f2f0;">| )
     ( LINES OF VALUE #( FOR code IN mt_code ( code ) ) )
     ( |</code></pre>| )
     ( |<script type="text/javascript">| )
     ( mv_prism_js )
     ( |</script>| )
     ( |</body>| )
     ( |</html>| )
    ).


    CONCATENATE LINES OF mt_output INTO lv_string SEPARATED BY cl_abap_char_utilities=>newline.

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


  METHOD on_close.
    mr_dialog_box->set_visible( cl_gui_control=>visible_false ).
  ENDMETHOD.


  METHOD zif_uitb_view~show.
    mr_dialog_box = NEW cl_gui_dialogbox_container(
        width   = 1400
        height  = 450
        top     = 50
        left    = 300
        caption = |{ COND #( WHEN mv_title IS NOT INITIAL THEN mv_title ELSE 'Code output'(001) ) }|
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
