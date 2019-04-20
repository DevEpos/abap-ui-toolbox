CLASS zcl_uitb_gui_code_editor DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_uitb_gui_control.

    CONSTANTS:
      BEGIN OF c_dnd_flavor,
        insert  TYPE cndd_flavor VALUE 'INSERT',
        replace TYPE cndd_flavor VALUE 'REPLACE',
      END OF c_dnd_flavor.

    ALIASES has_focus
      FOR zif_uitb_gui_control~has_focus.
    ALIASES focus
      FOR zif_uitb_gui_control~focus.

    EVENTS context_menu_selected
      EXPORTING
        VALUE(eo_menu) TYPE REF TO cl_ctmenu.

    "! <p class="shorttext synchronized" lang="en">Creates new instance of code editor</p>
    "!
    METHODS constructor
      IMPORTING
        io_parent      TYPE REF TO cl_gui_container
        iv_source_type TYPE string DEFAULT 'ABAP'
        iv_line_width  TYPE i DEFAULT 72.
    "! <p class="shorttext synchronized" lang="en">Format the source code of the editor</p>
    "!
    METHODS format_source.
    "! <p class="shorttext synchronized" lang="en">Sets the text of the editor</p>
    "!
    METHODS set_text
      IMPORTING
        iv_text TYPE string.
    "! <p class="shorttext synchronized" lang="en">Retrieve the selected text in the editor</p>
    "!
    METHODS get_selected_text
      RETURNING
        VALUE(rv_selected) TYPE string.
    "! <p class="shorttext synchronized" lang="en">Replace the selected text with the given text</p>
    "!
    METHODS set_selected_text
      IMPORTING
        value TYPE string.
    "! <p class="shorttext synchronized" lang="en">Get the current text in the editor</p>
    "!
    METHODS get_text
      RETURNING
        VALUE(rv_string) TYPE string.
    "! <p class="shorttext synchronized" lang="en">Select the given row in the editor</p>
    "!
    METHODS select_row
      IMPORTING
        iv_row TYPE i.
    "! <p class="shorttext synchronized" lang="en">Togge view/edit mode of editor</p>
    "!
    METHODS toggle_view_edit_mode.
    "! <p class="shorttext synchronized" lang="en">Sets the editor to editable</p>
    "!
    METHODS set_editable
      IMPORTING
        value TYPE abap_bool.

    "! <p class="shorttext synchronized" lang="en">Remove modified state from editor</p>
    "!
    METHODS set_unmodified.
    "! <p class="shorttext synchronized" lang="en">Returns the modified state of the editor</p>
    "!
    METHODS is_modified
      RETURNING
        VALUE(rf_modified) TYPE abap_bool.
    "! <p class="shorttext synchronized" lang="en">Protect the given lines</p>
    METHODS protect_lines
      IMPORTING
        it_lines TYPE cl_gui_sourceedit=>linetabs.
    "! <p class="shorttext synchronized" lang="en">Set cursor position to given line and column</p>
    METHODS set_sel_position
      IMPORTING
        iv_line TYPE i
        iv_pos  TYPE i DEFAULT 0.
    "! <p class="shorttext synchronized" lang="en">Gets the current cursor position</p>
    METHODS get_sel_position
      EXPORTING
        ev_line TYPE i
        ev_pos  TYPE i.

    "! <p class="shorttext synchronized" lang="en">Shows the given completion results</p>
    METHODS show_completion_results.
    "! <p class="shorttext synchronized" lang="en">Shows the given quick info</p>
    METHODS show_quick_info
      IMPORTING
        iv_info TYPE string.
  PROTECTED SECTION.
    METHODS on_drop
          FOR EVENT on_drop OF cl_gui_abapedit
      IMPORTING
          dragdrop_object
          index
          line
          pos.
    METHODS on_context_menu
          FOR EVENT context_menu OF cl_gui_abapedit
      IMPORTING
          menu
          menu_type
          sender.
    METHODS on_context_menu_selected
          FOR EVENT context_menu_selected OF cl_gui_abapedit
      IMPORTING
          fcode
          sender.
    METHODS on_f1
        FOR EVENT f1 OF cl_gui_abapedit.
    METHODS register_dnd_flavors
      IMPORTING
        io_dragdrop TYPE REF TO cl_dragdrop.
  PRIVATE SECTION.
    ALIASES mr_control
      FOR zif_uitb_gui_control~mr_control.

    DATA mv_read_only TYPE i.
    DATA mv_line_width TYPE i.
    DATA mt_text TYPE string_table.
    DATA mo_editor TYPE REF TO cl_gui_abapedit.
    DATA mo_dragdrop TYPE REF TO cl_dragdrop.
    METHODS init_control
      IMPORTING
        io_container   TYPE REF TO cl_gui_container
        iv_source_type TYPE string.
    METHODS register_events.
ENDCLASS.



CLASS zcl_uitb_gui_code_editor IMPLEMENTATION.
  METHOD constructor.
    DATA: lt_text TYPE STANDARD TABLE OF char140.

    mv_line_width = iv_line_width.
    init_control(
        io_container   = io_parent
        iv_source_type = iv_source_type
    ).
    register_events( ).

    mo_editor->set_text(
       table = lt_text
    ).
  ENDMETHOD.

  METHOD format_source.
    DATA: lt_source TYPE string_table.

    mo_editor->get_text(
      IMPORTING  table    = mt_text
      EXCEPTIONS OTHERS   = 1
    ).

*.. Prepend report
    lt_source = VALUE #(
      ( |REPORT ZTEST.| )
      ( LINES OF mt_text )
      ( |.| )
    ).

    DATA(lo_pretty_printer) = NEW cl_sedi_pretty_printer( ).
    TRY.
        lo_pretty_printer->format_source(
          EXPORTING
            i_settings   = NEW lcl_pretty_printer_settings( mv_line_width )
          CHANGING
            c_source     = lt_source
        ).

        DELETE lt_source INDEX 1.
        DELETE lt_source INDEX lines( lt_source ).

        mo_editor->set_text( table = lt_source ).
      CATCH cx_sedi_pretty_printer.
    ENDTRY.

  ENDMETHOD.

  METHOD protect_lines.
    CHECK mo_editor IS BOUND.

    mo_editor->protect_lines( protected_lines = it_lines ).
  ENDMETHOD.

  METHOD set_text.
    DATA: lt_text TYPE string_table.

    SPLIT iv_text AT cl_abap_char_utilities=>cr_lf INTO TABLE lt_text.
    mo_editor->set_text( lt_text ).
  ENDMETHOD.

  METHOD get_text.

    mo_editor->get_text(
        IMPORTING  table  = mt_text
        EXCEPTIONS OTHERS = 1 ).

    CONCATENATE LINES OF mt_text INTO rv_string SEPARATED BY cl_abap_char_utilities=>cr_lf.
  ENDMETHOD.

  METHOD get_selected_text.
    mo_editor->get_selected_text_as_stream(
      IMPORTING  selected_text = rv_selected
      EXCEPTIONS OTHERS        = 1
    ).
    IF sy-subrc <> 0.
    ENDIF.
  ENDMETHOD.

  METHOD set_selected_text.
    mo_editor->set_selected_text_as_stream( EXPORTING  selected_text = value
                                            EXCEPTIONS OTHERS        = 1 ).
  ENDMETHOD.

  METHOD select_row.
    mo_editor->select_lines( EXPORTING from_line = iv_row to_line = iv_row
                             EXCEPTIONS OTHERS = 1 ).
  ENDMETHOD.

  METHOD toggle_view_edit_mode.
    IF mv_read_only = cl_gui_textedit=>true.
      mv_read_only = cl_gui_textedit=>false.
    ELSE.
      mv_read_only = cl_gui_textedit=>true.
    ENDIF.

    mo_editor->set_readonly_mode( mv_read_only ).
  ENDMETHOD.

  METHOD zif_uitb_gui_control~focus.
    CHECK mo_editor IS BOUND.

    cl_gui_control=>set_focus( mo_editor ).
  ENDMETHOD.

  METHOD zif_uitb_gui_control~has_focus.
    CHECK mo_editor IS BOUND.

    cl_gui_control=>get_focus( IMPORTING control = mr_control ).
    rf_has_focus = xsdbool( mo_editor = mr_control ).
  ENDMETHOD.


  METHOD register_events.
    mo_editor->register_event_dblclick( EXPORTING navigate_on_dblclick = 1 EXCEPTIONS OTHERS = 1 ).

    DATA(lo_parser) = mo_editor->get_completer( ).
    IF lo_parser IS NOT INITIAL.
      mo_editor->register_event_completion( EXPORTING appl_event = abap_true EXCEPTIONS OTHERS = 1 ).
      IF sy-subrc = 0.
        SET HANDLER lo_parser->handle_completion_request for mo_editor.

        mo_editor->register_event_quick_info( EXPORTING appl_event = abap_false EXCEPTIONS OTHERS = 1 ).
        SET HANDLER lo_parser->handle_quickinfo_request FOR mo_editor.

        mo_editor->register_event_insert_pattern( EXPORTING appl_event = abap_false EXCEPTIONS OTHERS = 1 ).
        SET HANDLER lo_parser->handle_insertion_request FOR mo_editor.

*        mo_editor->register_event_f1( EXPORTING appl_event = abap_false EXCEPTIONS OTHERS = 1 ).
*        SET HANDLER on_f1 FOR mo_editor.
      ENDIF.

    ENDIF.

    mo_editor->register_event_context_menu( EXPORTING appl_event = abap_false EXCEPTIONS OTHERS = 1 ).

    SET HANDLER:
       on_context_menu FOR mo_editor,
       on_context_menu_selected FOR mo_editor,
       on_drop FOR mo_editor.

  ENDMETHOD.

  METHOD init_control.

    mo_editor = NEW #(
        parent           = io_container
        source_type      = iv_source_type
        max_number_chars = mv_line_width
    ).

    mo_editor->create_document( EXCEPTIONS OTHERS = 1 ).
    mo_editor->upload_properties( EXCEPTIONS OTHERS = 1 ).
    mo_editor->init_completer( ).

    mo_editor->set_tabbar_mode( tabbar_mode = cl_gui_abapedit=>false ).
    mo_editor->set_statusbar_mode( cl_gui_abapedit=>true ).

    mo_dragdrop = NEW #( ).

    register_dnd_flavors( EXPORTING io_dragdrop = mo_dragdrop ).

    mo_editor->set_dragdrop( mo_dragdrop ).
  ENDMETHOD.

  METHOD on_drop.
    TRY.
        DATA(lo_dnd_object) = CAST zcl_uitb_gui_editor_dnd_object( dragdrop_object->object ).
        CASE dragdrop_object->flavor.

          WHEN c_dnd_flavor-insert.
            mo_editor->set_selected_text_as_stream( selected_text = lo_dnd_object->get_text( ) ).

          WHEN c_dnd_flavor-replace.
            set_text( iv_text = lo_dnd_object->get_text( ) ).

        ENDCASE.
      CATCH cx_sy_move_cast_error.
    ENDTRY.

  ENDMETHOD.

  METHOD on_context_menu.
    menu->add_function(
       fcode       = 'ED_GOTO_LINE'
       text        = |{ 'Go to Line...' }|
       accelerator = 'O' ).

    RAISE EVENT context_menu_selected
      EXPORTING
        eo_menu = menu.

    focus( ).
  ENDMETHOD.

  METHOD on_context_menu_selected.
    focus( ).
  ENDMETHOD.


  METHOD register_dnd_flavors.
    mo_dragdrop->add(
        flavor     = c_dnd_flavor-insert
        dragsrc    = abap_false
        droptarget = abap_true
    ).

    mo_dragdrop->add(
        flavor     = c_dnd_flavor-replace
        dragsrc    = abap_false
        droptarget = abap_true
    ).

  ENDMETHOD.

  METHOD on_f1.
    DATA: lt_tokens TYPE abapdocu_tokens.

*    cl_gui_cfw=>set_new_ok_code( EXPORTING new_code = 'HELP' ).

    mo_editor->get_selection_pos(
      IMPORTING  from_line = DATA(lv_from_line)
                 from_pos  = DATA(lv_from_pos)
                 to_line   = DATA(lv_to_line)
                 to_pos    = DATA(lv_to_pos)
      EXCEPTIONS OTHERS    = 1
    ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    mo_editor->get_selected_text_as_stream( IMPORTING selected_text = DATA(lv_keyword) EXCEPTIONS OTHERS = 1 ).

    mo_editor->send_completion_command(
      EXPORTING row    = lv_from_line
                column = lv_from_pos
      IMPORTING tokens = lt_tokens ).

    cl_abap_docu=>start(
      EXPORTING
        word           = to_upper( lv_keyword )
*        first_word     = to_upper( lv_keyword )
*        tokens         = lt_tokens
        mode           = 'ABAP'
      EXCEPTIONS
        OTHERS         = 1 ).
  ENDMETHOD.

  METHOD set_editable.
    mo_editor->set_readonly_mode( COND #( WHEN value = abap_true THEN cl_gui_abapedit=>false ELSE cl_gui_abapedit=>true ) ).
  ENDMETHOD.

  METHOD set_unmodified.
    mo_editor->set_textmodified_status( EXCEPTIONS OTHERS = 1 ).
  ENDMETHOD.

  METHOD is_modified.
    mo_editor->get_textmodified_status( IMPORTING status = DATA(lv_status) EXCEPTIONS OTHERS = 1 ).
    rf_modified = xsdbool( lv_status = cl_gui_abapedit=>true ).
  ENDMETHOD.




  METHOD get_sel_position.
    mo_editor->get_selection_pos( IMPORTING from_line = ev_line
                                            from_pos  = ev_pos
                                  EXCEPTIONS OTHERS = 1 ).
  ENDMETHOD.

  METHOD set_sel_position.
    mo_editor->set_selection_pos_in_line(
      EXPORTING
        line    = iv_line
        pos     = iv_pos
      EXCEPTIONS
        OTHERS  = 1
    ).
  ENDMETHOD.

  METHOD show_completion_results.
    DATA: lt_source TYPE string_table.

    get_sel_position(
      IMPORTING
        ev_line = DATA(lv_line)
        ev_pos  = DATA(lv_pos)
    ).
    mo_editor->get_text( IMPORTING table = lt_source EXCEPTIONS OTHERS = 1 ).

    DATA(lt_completion_results) = zcl_uitb_code_completer=>get_completion_results(
      it_source = lt_source
      iv_pos_x  = lv_pos - 1
      iv_pos_y  = lv_line
    ).
    mo_editor->show_completion_results(
        completion_results = lt_completion_results
    ).
  ENDMETHOD.

  METHOD show_quick_info.
    mo_editor->show_quick_info( iv_info ).
  ENDMETHOD.

ENDCLASS.
