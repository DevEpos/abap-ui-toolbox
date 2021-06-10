"! <p class="shorttext synchronized" lang="en">ABAP CodeMirror editor</p>
CLASS zcl_uitb_abap_cm DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_uitb_gui_control.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      ty_w3_mimes  TYPE STANDARD TABLE OF w3_mime WITH EMPTY KEY,
      ty_mime_type TYPE c LENGTH 20.

    CONSTANTS:
      BEGIN OF c_mime_types,
        text TYPE ty_mime_type VALUE 'text',
      END OF c_mime_types,
      BEGIN OF c_mime_subtypes,
        css        TYPE ty_mime_type VALUE 'css',
        javascript TYPE ty_mime_type VALUE 'javascript',
        html       TYPE ty_mime_type VALUE 'html',
      END OF c_mime_subtypes,
      BEGIN OF c_mime_paths,
        BEGIN OF js,
          codemirror_main            TYPE string VALUE '/sap/public/zuitb/cm_lib_codemirror.js',
          codemirror_mode_abap       TYPE string VALUE '/sap/public/zuitb/cm_mode_abap.min.js',
          codemirror_addon_show_hint TYPE string VALUE '/sap/public/zuitb/cm_addon_hint_show-hint.js',
          thirdparty_es6_promise     TYPE string VALUE '/sap/public/zuitb/thirdparty_es6_promise.js',
        END OF js,
        BEGIN OF css,
          codemirror_main               TYPE string VALUE '/sap/public/zuitb/cm_lib_codemirror.css',
          codemirror_addon_show_hint    TYPE string VALUE '/sap/public/zuitb/cm_addon_hint_show-hint.css',
          codemirror_theme_abcdef       TYPE string VALUE '/sap/public/zuitb/cm_theme_abcdef.css',
          codemirror_theme_matrl_darker TYPE string VALUE '/sap/public/zuitb/cm_theme_material-darker.css',
        END OF css,
        BEGIN OF html,
          codemirror_abap TYPE string VALUE '/sap/public/zuitb/codemirror_abap.html',
        END OF html,
      END OF c_mime_paths,

      BEGIN OF c_mime_urls,
        BEGIN OF js,
          codemirror_main            TYPE string VALUE 'lib/codemirror.js',
          codemirror_mode_abap       TYPE string VALUE 'mode/abap/abap.min.js',
          codemirror_addon_show_hint TYPE string VALUE 'addon/hint/show-hint-js',
          thirdparty_es6_promise     TYPE string VALUE 'lib/thirdparty/es6_promise.js',
        END OF js,
        BEGIN OF css,
          codemirror_main               TYPE string VALUE 'lib/codemirror.css',
          codemirror_addon_show_hint    TYPE string VALUE 'addon/hint/show-hint.css',
          codemirror_theme_abcdef       TYPE string VALUE 'theme/abcdef.css',
          codemirror_theme_matrl_darker TYPE string VALUE 'theme/material-darker.css',
        END OF css,
      END OF c_mime_urls.


    DATA:
      mo_html_viewer   TYPE REF TO cl_gui_html_viewer,
      mt_errors        TYPE string_table,
      mo_dummy_control TYPE REF TO cl_gui_control.

    METHODS:
      get_mime_raw
        IMPORTING
          iv_path TYPE c
        EXPORTING
          et_raw  TYPE ty_w3_mimes
          ev_size TYPE i,
      load_mime
        IMPORTING
          iv_path         TYPE c
          iv_assigned_url TYPE c
          iv_type         TYPE ty_mime_type DEFAULT c_mime_types-text
          iv_subtype      TYPE ty_mime_type DEFAULT c_mime_subtypes-html,
      create_document.
ENDCLASS.



CLASS zcl_uitb_abap_cm IMPLEMENTATION.


  METHOD zif_uitb_gui_control~focus.
    CHECK mo_html_viewer IS BOUND.

    cl_gui_control=>set_focus( EXPORTING control = mo_html_viewer EXCEPTIONS OTHERS = 1 ).
  ENDMETHOD.


  METHOD zif_uitb_gui_control~has_focus.
    CHECK mo_html_viewer IS BOUND.

    cl_gui_control=>get_focus( IMPORTING control = mo_dummy_control EXCEPTIONS OTHERS = 1 ).
    rf_has_focus = xsdbool( mo_dummy_control = mo_html_viewer ).
  ENDMETHOD.


  METHOD create_document.
    DATA: lv_string TYPE string,
          lv_xstr   TYPE xstring.

    " TODO: Read html for abap codemirror
    " TODO: set theme url
  ENDMETHOD.


  METHOD get_mime_raw.
    cl_mime_repository_api=>get_api( )->get(
     EXPORTING i_url = iv_path
     IMPORTING e_content = DATA(lv_mime_raw)
     EXCEPTIONS OTHERS = 1 ).

    IF sy-subrc = 0.
      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer        = lv_mime_raw
        IMPORTING
          output_length = ev_size
        TABLES
          binary_tab    = et_raw.
    ENDIF.
  ENDMETHOD.


  METHOD load_mime.

    get_mime_raw(
      EXPORTING iv_path = iv_path
      IMPORTING et_raw  = DATA(lt_raw_content)
                ev_size = DATA(lv_mime_size) ).

    IF lt_raw_content IS NOT INITIAL.
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



ENDCLASS.
