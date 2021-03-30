CLASS zcl_uitb_app_shortcuts_viewer DEFINITION
  PUBLIC
  INHERITING FROM zcl_uitb_gui_modal_dialog
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS class_constructor.
    "! <p class="shorttext synchronized" lang="en">Display dialog with all registered F-Key in the screen</p>
    "!
    CLASS-METHODS display_shortcuts
      IMPORTING
        it_shortcuts TYPE zif_uitb_ty_gui_screen=>ty_t_fkey_map.
    METHODS zif_uitb_gui_command_handler~execute_command
        REDEFINITION.
  PROTECTED SECTION.
    METHODS create_content
        REDEFINITION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_s_fkey_sort,
             fkey     TYPE ui_func,
             sort_key TYPE i,
           END OF ty_s_fkey_sort.
    TYPES: BEGIN OF ty_s_fkey_map_with_sort.
             INCLUDE TYPE zif_uitb_ty_gui_screen=>ty_s_fkey_map.
    TYPES:   sort_key TYPE i.
    TYPES: END OF ty_s_fkey_map_with_sort.

    DATA mt_output TYPE string_table.
    CLASS-DATA gt_fkey_sort TYPE STANDARD TABLE OF ty_s_fkey_sort WITH KEY fkey.
    DATA mt_shortcuts TYPE STANDARD TABLE OF ty_s_fkey_map_with_sort.

    METHODS constructor
      IMPORTING
        it_shortcuts TYPE zif_uitb_ty_gui_screen=>ty_t_fkey_map.


    TYPES: tt_w3_mime TYPE STANDARD TABLE OF w3_mime WITH DEFAULT KEY.
    "! <p class="shorttext synchronized" lang="en">HTML Control Proxy Class</p>
    DATA mr_html_viewer TYPE REF TO cl_gui_html_viewer .
    "! <p class="shorttext synchronized" lang="en">Text (80 Characters)</p>
    DATA mv_html_url TYPE text80 .
    DATA mv_html_size TYPE i.
    "! <p class="shorttext synchronized" lang="en">ITS: Table with MIMEs</p>
    DATA mt_html_raw TYPE tt_w3_mime.
    DATA mo_shortcuts_content TYPE REF TO zcl_uitb_html_content.
    "! <p class="shorttext synchronized" lang="en">Create HTML document for displaying the code</p>
    "!
    METHODS create_document.
    METHODS build_shortcuts_content.
    METHODS get_shortcut_text
      IMPORTING
        iv_shortcut_key         TYPE ui_func
      RETURNING
        VALUE(rv_shortcut_text) TYPE string.
ENDCLASS.



CLASS zcl_uitb_app_shortcuts_viewer IMPLEMENTATION.
  METHOD display_shortcuts.
    DATA(lr_code_viewer) = NEW zcl_uitb_app_shortcuts_viewer(
      it_shortcuts = it_shortcuts ).

    lr_code_viewer->build_shortcuts_content( ).
    lr_code_viewer->create_document( ).
    lr_code_viewer->show(
      iv_top    = 5
      iv_left   = 30
      iv_width  = 100
      iv_height = 20 ).
  ENDMETHOD.

  METHOD constructor.
    super->constructor( iv_title = |{ 'Available Shortcuts on this Screen'(003) }| ).
    mt_shortcuts = CORRESPONDING #( it_shortcuts ).

*.. Sort the short cuts
    LOOP AT mt_shortcuts  ASSIGNING FIELD-SYMBOL(<ls_shortcut>).
      <ls_shortcut>-sort_key = VALUE #( gt_fkey_sort[ fkey = <ls_shortcut>-fkey ]-sort_key OPTIONAL ).
    ENDLOOP.

    SORT mt_shortcuts BY sort_key.
  ENDMETHOD.

  METHOD class_constructor.
    gt_fkey_sort = VALUE #(
      ( fkey = zif_uitb_c_gui_screen=>c_functions-f2             )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-f5             )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-f6             )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-f7             )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-f8             )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-f9             )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-shift_f1       )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-shift_f2       )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-shift_f4       )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-shift_f5       )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-shift_f6       )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-shift_f7       )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-shift_f8       )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-shift_f9       )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-shift_f11      )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-shift_f12      )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-ctrl_f1        )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-ctrl_f2        )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-ctrl_f3        )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-ctrl_f4        )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-ctrl_f5        )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-ctrl_f6        )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-ctrl_f7        )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-ctrl_f8        )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-ctrl_f9        )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-ctrl_f10       )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-ctrl_f11       )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-ctrl_f12       )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-ctrl_shift_f2  )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-ctrl_shift_f3  )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-ctrl_shift_f4  )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-ctrl_shift_f5  )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-ctrl_shift_f6  )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-ctrl_shift_f7  )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-ctrl_shift_f8  )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-ctrl_shift_f9  )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-ctrl_shift_f10 )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-ctrl_shift_f11 )
      ( fkey = zif_uitb_c_gui_screen=>c_functions-ctrl_shift_f12 ) ).
    LOOP AT gt_fkey_sort ASSIGNING FIELD-SYMBOL(<ls_fkey_sort>).
      <ls_fkey_sort>-sort_key = sy-tabix.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_uitb_gui_command_handler~execute_command.
    RETURN.
  ENDMETHOD.

  METHOD create_content.
    mr_html_viewer = NEW #(
        parent               = io_container
        query_table_disabled = abap_true ).

    " load html code into control
    mr_html_viewer->load_data(
      EXPORTING
        size         = mv_html_size    " Length of Data
      IMPORTING
        assigned_url = mv_html_url    " URL
      CHANGING
        data_table   = mt_html_raw    " data table
      EXCEPTIONS
        OTHERS       = 1 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_uitb_gui_exception.
    ENDIF.

    mr_html_viewer->show_data(
      EXPORTING
        url      = mv_html_url    " URL
      EXCEPTIONS
        OTHERS   = 1 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_uitb_gui_exception.
    ENDIF.
  ENDMETHOD.

  METHOD build_shortcuts_content.
    mo_shortcuts_content = NEW #( ).

    mo_shortcuts_content->start_table(
      )->start_table_row(
      )->add_table_header_cell(
        iv_text = |{ 'Shortcut'(001) }|
      )->add_table_header_cell(
        iv_text = |{ 'Defined User function'(002) }|
      )->end_table_row( ).

    LOOP AT mt_shortcuts ASSIGNING FIELD-SYMBOL(<ls_shortcut>).
      mo_shortcuts_content->start_table_row(
        )->add_table_body_cell(
          iv_text = |<code>{ get_shortcut_text( <ls_shortcut>-fkey ) }</code>|
        )->add_table_body_cell(
          iv_text = |{ <ls_shortcut>-text }|
        )->end_table_row( ).
    ENDLOOP.

    mo_shortcuts_content->end_table( ).
  ENDMETHOD.

  METHOD get_shortcut_text.
    DATA: lt_shortcut_parts TYPE string_table.

    SPLIT iv_shortcut_key AT '_' INTO TABLE lt_shortcut_parts.

    LOOP AT lt_shortcut_parts INTO DATA(lv_part).
      IF rv_shortcut_text IS NOT INITIAL.
        rv_shortcut_text = |{ rv_shortcut_text }+|.
      ENDIF.

      rv_shortcut_text = rv_shortcut_text &&
        SWITCH #( lv_part
          WHEN 'S' THEN |{ 'Shift' }|
          WHEN 'C' THEN |{ 'Ctrl' }|
          ELSE lv_part ).
    ENDLOOP.
  ENDMETHOD.

  METHOD create_document.
    DATA: lv_string TYPE string,
          lv_xstr   TYPE xstring.

    mt_output = VALUE #(
     ( |<!DOCTYPE html>| )
     ( |<html>| )
     ( |<head>| )
     ( |<meta charset="utf-8">| )
     ( |<style type="text/css">| )
     ( | body \{| )
     ( |     font-family: -apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji,Segoe UI Symbol;| )
     ( |     font-size: 14px;| )
     ( |     line-height: 1.5;   | )
     ( | \}| )
     ( | table \{| )
     ( |     width: 100%;| )
     ( |     border-collapse: collapse;| )
     ( |     border-spacing: 0;| )
     ( | \}| )
     ( | table td,| )
     ( | table th \{| )
     ( |     border: 1px solid #dfe2e5;| )
     ( |     padding: 6px 13px;| )
     ( | \}| )
     ( | | )
     ( | table th \{| )
     ( |     background-color: #f5f5f5;| )
     ( | \}| )
     ( | code \{| )
     ( |     background-color: rgba(27,31,35,.05);| )
     ( |     border-radius: 3px;| )
     ( |     font-size: 85%;| )
     ( |     margin: 0;| )
     ( |     padding: .2em .4em;| )
     ( |     font-family: SFMono-Regular,Consolas,Liberation Mono,Menlo,Courier,monospace;| )
     ( | \}| )
     ( |</style>| )
     ( |</head>| )
     ( |<body>| )
     ( LINES OF VALUE #( FOR html IN mo_shortcuts_content->get_content( ) ( |{ html-line }| ) ) )
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


ENDCLASS.
