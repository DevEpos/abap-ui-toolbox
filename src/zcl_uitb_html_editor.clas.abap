CLASS zcl_uitb_html_editor DEFINITION
   PUBLIC
  CREATE PRIVATE .

  PUBLIC SECTION.

    EVENTS html_updated
      EXPORTING
        VALUE(html) TYPE string
        VALUE(text) TYPE string.

    CLASS-METHODS create
      IMPORTING
        !ir_container    TYPE REF TO cl_gui_container
      RETURNING
        VALUE(rv_editor) TYPE REF TO zcl_uitb_html_editor .
    METHODS show
      IMPORTING
        VALUE(iv_html)   TYPE string OPTIONAL
        VALUE(iv_height) TYPE i DEFAULT 400
        VALUE(iv_width)  TYPE i DEFAULT 800 .
    METHODS get_html
      RETURNING
        VALUE(rv_html) TYPE string .
    METHODS get_text
      RETURNING
        VALUE(result) TYPE string.
  PROTECTED SECTION.

    DATA mr_nice_html_editor TYPE REF TO cl_gui_html_viewer .

    METHODS on_sapevent
          FOR EVENT sapevent OF cl_gui_html_viewer
      IMPORTING
          !action
          !frame
          !getdata
          !postdata
          !query_table .
  PRIVATE SECTION.

    DATA mv_height TYPE string .
    DATA mv_width TYPE string .
    DATA mf_initial_show TYPE abap_bool VALUE abap_true ##NO_TEXT.
    DATA mv_assigned_url TYPE c LENGTH 2048 .
    DATA mv_current_html TYPE string .
    DATA mv_current_text TYPE string.

    CONSTANTS c_mime_path TYPE string VALUE '/SAP/PUBLIC/ZUITB/html_editor.html' ##NO_TEXT.

    METHODS constructor
      IMPORTING
        !io_container TYPE REF TO cl_gui_container .
    METHODS get_editor_html
      IMPORTING
        VALUE(iv_content)     TYPE string
      RETURNING
        VALUE(rv_editor_soli) TYPE soli_tab .
    METHODS show_editor
      IMPORTING
        VALUE(if_initial) TYPE abap_bool DEFAULT abap_false
      CHANGING
        !ct_soli          TYPE soli_tab .
ENDCLASS.



CLASS zcl_uitb_html_editor IMPLEMENTATION.


  METHOD constructor.
    CREATE OBJECT mr_nice_html_editor
      EXPORTING
        parent = io_container.

  ENDMETHOD.


  METHOD create.
    CREATE OBJECT rv_editor
      EXPORTING
        io_container = ir_container.

  ENDMETHOD.


  METHOD get_editor_html.
    DATA: mv_editor_x      TYPE xstring,
          mv_editor_string TYPE string.

    cl_mime_repository_api=>get_api( )->get(
     EXPORTING i_url = c_mime_path
     IMPORTING e_content = mv_editor_x
     EXCEPTIONS OTHERS = 1 ).

    CHECK sy-subrc = 0.

    DATA(lr_converter) = cl_abap_conv_in_ce=>create(
      encoding = 'UTF-8'
      input    = mv_editor_x
    ).

    lr_converter->read( IMPORTING data = mv_editor_string ).

    REPLACE ALL OCCURRENCES OF '&TextToReplace&' IN mv_editor_string WITH iv_content.
    REPLACE ALL OCCURRENCES OF '&&height&&' IN mv_editor_string WITH mv_height.
    REPLACE ALL OCCURRENCES OF '&&width&&' IN mv_editor_string WITH mv_width.

    rv_editor_soli =  cl_bcs_convert=>string_to_soli( iv_string = mv_editor_string ).
  ENDMETHOD.


  METHOD get_html.
    rv_html = mv_current_html.
  ENDMETHOD.


  METHOD on_sapevent.
    DATA: lv_html            TYPE string,
          lv_text            TYPE string,
          lt_sapoffice_lines TYPE soli_tab.

    CHECK action = 'onSave'.

    LOOP AT query_table ASSIGNING FIELD-SYMBOL(<query>).
      IF <query>-name CP 'text*'.
        lv_text = lv_text && <query>-value.
      ELSE.
        lv_html = lv_html && <query>-value.
      ENDIF.
    ENDLOOP.

    "update html
    IF mv_current_html NE lv_html.
      mv_current_html = lv_html.
      mv_current_text = lv_text.
      RAISE EVENT html_updated
        EXPORTING
          html = lv_html
          text = lv_text.
    ENDIF.

    "as there was post method, we need to refresh
    "html viewer with our html code
    lt_sapoffice_lines = get_editor_html( iv_content = lv_html ).
    show_editor( CHANGING ct_soli = lt_sapoffice_lines  ).

  ENDMETHOD.


  METHOD show.
    DATA: soli TYPE soli_tab.

    IF mf_initial_show = abap_true OR iv_height IS SUPPLIED AND iv_width IS SUPPLIED.
      mv_height = |{ iv_height }px|.
      mv_width = |{ iv_width }px|.
    ENDIF.

    "as there was post method, we need to refresh
    "html viewer with our html code
    mv_current_html = iv_html.
    soli = get_editor_html( iv_content = iv_html ).
    show_editor(
        EXPORTING if_initial = mf_initial_show
        CHANGING  ct_soli    = soli
    ).
  ENDMETHOD.


  METHOD show_editor.
    DATA:  events TYPE cntl_simple_events,
           event  TYPE cntl_simple_event.

    mr_nice_html_editor->load_data(
     EXPORTING
       url                    = mv_assigned_url
       type                   = 'text'   " Type of a MIME Object
       subtype                = 'html'    " Subtype of a MIME Object
     IMPORTING
       assigned_url           = mv_assigned_url    " URL
     CHANGING
       data_table             = ct_soli   " data table
     EXCEPTIONS
       dp_invalid_parameter   = 1
       dp_error_general       = 2
       cntl_error             = 3
       html_syntax_notcorrect = 4
       OTHERS                 = 5
    ).
    IF sy-subrc <> 0.
*   message id sy-msgid type sy-msgty number sy-msgno
*              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF if_initial = abap_true.
      mf_initial_show = abap_false.
      " register event
      event-eventid = mr_nice_html_editor->m_id_sapevent.
      event-appl_event = 'x'.
      APPEND event TO events.

      mr_nice_html_editor->set_registered_events( events ).

      SET HANDLER on_sapevent FOR mr_nice_html_editor.

    ENDIF.

    mr_nice_html_editor->show_url( mv_assigned_url ).
  ENDMETHOD.

  METHOD get_text.
    result = mv_current_text.
  ENDMETHOD.

ENDCLASS.
