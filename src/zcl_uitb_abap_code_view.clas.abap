CLASS zcl_uitb_abap_code_view DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_uitb_gui_control.

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        io_parent_view      TYPE REF TO zif_uitb_composite_view OPTIONAL
        iv_theme            TYPE zuitb_code_viewer_theme DEFAULT zif_uitb_c_code_viewer_themes=>default
        io_parent_container TYPE REF TO cl_gui_container.

    "! <p class="shorttext synchronized" lang="en">Shows the given ABAP code in the html control</p>
    METHODS show_document
      IMPORTING
        it_code TYPE string_table OPTIONAL
        iv_code TYPE string OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_mime_urls,
        prism_js  TYPE c LENGTH 30 VALUE 'js/prism.js' ##NO_TEXT,
        prism_css TYPE c LENGTH 30 VALUE 'css/prism.css' ##NO_TEXT,
      END OF c_mime_urls,
      BEGIN OF c_mime_paths,
        prism_js  TYPE c LENGTH 30 VALUE '/sap/public/zuitb/prism.js' ##NO_TEXT,
        prism_css TYPE c LENGTH 30 VALUE '/sap/public/zuitb/prism.css' ##NO_TEXT,
      END OF c_mime_paths.

    DATA mr_dummy_control TYPE REF TO cl_gui_control.
    DATA mt_code TYPE string_table.
    DATA mt_errors TYPE string_table.
    DATA mt_output TYPE string_table.
    DATA mo_parent_view TYPE REF TO zif_uitb_composite_view.
    TYPES: tt_w3_mime TYPE STANDARD TABLE OF w3_mime WITH DEFAULT KEY.
    "! <p class="shorttext synchronized" lang="en">HTML Control Proxy Class</p>
    DATA mo_html_viewer TYPE REF TO cl_gui_html_viewer .
    "! <p class="shorttext synchronized" lang="en">Text (80 Characters)</p>
    DATA mv_html_url TYPE text80 .
    DATA mv_html_size TYPE i.
    "! <p class="shorttext synchronized" lang="en">ITS: Table with MIMEs</p>
    DATA mt_html_raw TYPE tt_w3_mime.
    DATA mv_theme TYPE zuitb_code_viewer_theme.

    "! <p class="shorttext synchronized" lang="en">Load mime data for path</p>
    "!
    METHODS load_mime
      IMPORTING
        iv_path         TYPE c
        iv_assigned_url TYPE c
        iv_type         TYPE c DEFAULT 'text'
        iv_subtype      TYPE c DEFAULT 'html'.
    "! <p class="shorttext synchronized" lang="en">Load prism library from mime</p>
    "!
    METHODS load_prism.
    "! <p class="shorttext synchronized" lang="en">Create HTML document for displaying the code</p>
    "!
    METHODS create_document.
    "! <p class="shorttext synchronized" lang="en">Create html viewer control for code output</p>
    METHODS create_control
      IMPORTING
        io_parent TYPE REF TO cl_gui_container.
    METHODS get_theme_class
      RETURNING
        VALUE(rv_theme_class) TYPE string.
ENDCLASS.



CLASS zcl_uitb_abap_code_view IMPLEMENTATION.

  METHOD constructor.
    mo_parent_view = io_parent_view.
    mv_theme = iv_theme.
    create_control( io_parent_container ).
    load_prism( ).
  ENDMETHOD.

  METHOD show_document.
    DATA(lv_prism_pre_tag_start) = |<pre><code class="language-abap">|.

    IF iv_code IS NOT INITIAL.
      mt_code = VALUE #( ( lv_prism_pre_tag_start && iv_code ) ).
    ELSEIF it_code IS NOT INITIAL.
      mt_code = it_code.
      mt_code[ 1 ] = lv_prism_pre_tag_start && mt_code[ 1 ].
    ENDIF.

    create_document( ).

    CHECK mt_html_raw IS NOT INITIAL.

    mo_html_viewer->load_data(
      EXPORTING
        size         = mv_html_size    " Length of Data
      IMPORTING
        assigned_url = mv_html_url    " URL
      CHANGING
        data_table   = mt_html_raw    " data table
      EXCEPTIONS
        OTHERS       = 1
    ).

    CHECK mv_html_url IS NOT INITIAL.

    mo_html_viewer->show_data(
      EXPORTING
        url      = mv_html_url    " URL
      EXCEPTIONS
        OTHERS   = 1
    ).
  ENDMETHOD.

  METHOD zif_uitb_gui_control~focus.
    CHECK mo_html_viewer IS BOUND.

    cl_gui_control=>set_focus( EXPORTING control = mo_html_viewer EXCEPTIONS OTHERS = 1 ).
  ENDMETHOD.

  METHOD zif_uitb_gui_control~has_focus.
    CHECK mo_html_viewer IS BOUND.

    cl_gui_control=>get_focus( IMPORTING control = mr_dummy_control EXCEPTIONS OTHERS = 1 ).
    rf_has_focus = xsdbool( mr_dummy_control = mo_html_viewer ).
  ENDMETHOD.

  METHOD create_control.
    mo_html_viewer = NEW #(
        parent               = io_parent
        query_table_disabled = abap_true
    ).
  ENDMETHOD.

  METHOD load_prism.
    load_mime(
      iv_path         = c_mime_paths-prism_css
      iv_assigned_url = c_mime_urls-prism_css
      iv_type         = 'text'
      iv_subtype      = 'css' ).
    load_mime(
      iv_path         = c_mime_paths-prism_js
      iv_assigned_url = c_mime_urls-prism_js
      iv_type         = 'text'
      iv_subtype      = 'javascript' ).
  ENDMETHOD.

  METHOD load_mime.
    DATA: lt_raw_content TYPE tt_w3_mime,
          lv_mime_size   TYPE i.

    cl_mime_repository_api=>get_api( )->get(
     EXPORTING i_url = iv_path
     IMPORTING e_content = DATA(lv_mime_raw)
     EXCEPTIONS OTHERS = 1 ).

    IF sy-subrc = 0.
      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer        = lv_mime_raw
        IMPORTING
          output_length = lv_mime_size
        TABLES
          binary_tab    = lt_raw_content.

      mo_html_viewer->load_data(
        EXPORTING
          url                    = iv_assigned_url
          type                   = iv_type
          subtype                = iv_subtype
          size                   = lv_mime_size
        CHANGING
          data_table             = lt_raw_content
        EXCEPTIONS
          dp_invalid_parameter   = 1
          dp_error_general       = 2
          cntl_error             = 3
          html_syntax_notcorrect = 4
          OTHERS                 = 5 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ELSE.
      APPEND |<p style="color: red; font-family: 'Consolas';">|
        && |MIME object at path <span style="text-decoration: underline">{ iv_path }</span>|
        && | with type <span style="text-decoration: underline">{ iv_type }/{ iv_subtype }</span>|
        && | is not available in the MIME repository!|
        && |</p>| TO mt_errors.
    ENDIF.
  ENDMETHOD.

  METHOD create_document.
    DATA: lv_string TYPE string,
          lv_xstr   TYPE xstring.

    mt_output = VALUE #(
     ( |<!DOCTYPE html>| )
     ( |<html class="{ get_theme_class( ) }">| )
     ( |<head>| )
     ( |<meta charset="utf-8">| )
     ( |<link href="css/prism.css" rel="stylesheet" type="text/css"/>| )
     ( |</head>| )
     ( |<body>| )
     ( LINES OF VALUE #( FOR error IN mt_errors ( error ) ) )
     ( LINES OF VALUE #( FOR code IN mt_code ( code ) ) )
     ( |</code></pre>| )
     ( |<script src="js/prism.js" type="text/javascript"></script>| )
     ( |</body>| )
     ( |</html>| ) ).

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


  METHOD get_theme_class.
    rv_theme_class = SWITCH #(
      mv_theme
      WHEN zif_uitb_c_code_viewer_themes=>default   THEN 'prism-default'
      WHEN zif_uitb_c_code_viewer_themes=>coy       THEN 'prism-coy'
      WHEN zif_uitb_c_code_viewer_themes=>a11y_dark THEN 'prism-a11y-dark'
      ELSE                                               'prism-default'
    ).
  ENDMETHOD.

ENDCLASS.
