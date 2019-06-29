"! <p class="shorttext synchronized" lang="en">Selection/Filter Dialog with ALV</p>
CLASS zcl_uitb_selection_dialog DEFINITION
  PUBLIC
  INHERITING FROM zcl_uitb_gui_modal_dialog
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS: c_mark_default_field TYPE fieldname VALUE 'MARK',
               c_filtered_field     TYPE fieldname VALUE 'FILTERED'.

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        iv_title          TYPE string
        iv_filter_prompt  TYPE string OPTIONAL
        if_multi_select   TYPE abap_bool OPTIONAL
        if_use_alv_filter TYPE abap_bool OPTIONAL.
    METHODS zif_uitb_gui_command_handler~execute_command
        REDEFINITION.
  PROTECTED SECTION.
    CONSTANTS: c_func_focus_input       TYPE ui_func VALUE 'FOCUS_INPUT',
               c_func_focus_alv         TYPE ui_func VALUE 'FOCUS_ALV',
               c_func_accept_selections TYPE ui_func VALUE 'ACCEPT_VALUES',
               c_func_select_all        TYPE ui_func VALUE 'SELECT_ALL',
               c_func_unselect_all      TYPE ui_func VALUE 'UNSELECT_ALL'.
    DATA mo_alv TYPE REF TO zcl_uitb_alv.
    DATA mr_t_data TYPE REF TO data.
    DATA mf_use_alv_filter TYPE abap_bool.
    DATA mf_multi_select TYPE abap_bool.
    DATA mo_filter_input TYPE REF TO cl_gui_input_field.
    DATA mf_data_selected TYPE abap_bool.

    "! <p class="shorttext synchronized" lang="en">Returns the field used for Filtering the values</p>
    "!
    "! To be redefined by subclass if multi selections are enabled and <br/>
    "! another field named 'FILTERED' should be used for filtering rows
    METHODS get_filtered_column
      RETURNING
        VALUE(rv_field) TYPE fieldname.
    "! <p class="shorttext synchronized" lang="en">Returns marking field in the output structure</p>
    "!
    "! To be redefined by subclass if multi selections are enabled and <br/>
    "! another field named 'MARK' should be used for remembering the selection <br/>
    "! status of the row
    METHODS get_mark_field
      RETURNING
        VALUE(rv_field) TYPE fieldname.
    "! <p class="shorttext synchronized" lang="en">Returns 'X' if selections were made</p>
    "! To be redefined in subclass if multi selections are enabled
    METHODS has_selections
      RETURNING
        VALUE(rf_has_selections) TYPE abap_bool.
    METHODS do_before_dynpro_output
        REDEFINITION.
    "! <p class="shorttext synchronized" lang="en">Creates the output table</p>
    "! To be implemented by subclasses <br/>
    "! <strong>Example</strong>: <br/>
    "! rr_table = new [table_type]( ).
    METHODS get_output_table ABSTRACT
      RETURNING
        VALUE(rr_table) TYPE REF TO data.
    METHODS create_content
        REDEFINITION.
    "! <p class="shorttext synchronized" lang="en">Create ALV for variant display</p>
    METHODS create_alv
      IMPORTING
        io_container TYPE REF TO cl_gui_container.
    "! <p class="shorttext synchronized" lang="en">Adjust the settings of the given column</p>
    "!
    "! To be redefined by sub classes if columns need to be adjusted
    METHODS adjust_column
      IMPORTING
        io_column TYPE REF TO zcl_uitb_alv_column.
    "! <p class="shorttext synchronized" lang="en">Finds variants for the current entity and filter value</p>
    METHODS get_filtered_data
      IMPORTING
        iv_name_filter TYPE string OPTIONAL
      RETURNING
        VALUE(rr_data) TYPE REF TO data.
    "! <p class="shorttext synchronized" lang="en">Check if the given data matches the passed filter</p>
    METHODS matches_filter
      IMPORTING
        iv_filter         TYPE string
        is_data           TYPE any
      RETURNING
        VALUE(rf_matches) TYPE abap_bool.
    "! <p class="shorttext synchronized" lang="en">Sets the selected element</p>
    "! To be redefined by sub classes
    METHODS set_selected_element ABSTRACT
      IMPORTING
        iv_row    TYPE i
        iv_column TYPE lvc_fname.
    "! <p class="shorttext synchronized" lang="en">Handler for submit event of filter input</p>
    METHODS on_filter_submit
          FOR EVENT submit OF cl_gui_input_field
      IMPORTING
          input.
    "! <p class="shorttext synchronized" lang="en">Handler for ALV link click</p>
    METHODS on_alv_link_click
          FOR EVENT link_click OF zcl_uitb_alv_events
      IMPORTING
          ev_column
          ev_row.
    "! <p class="shorttext synchronized" lang="en">Handler for ALV double click</p>
    METHODS on_alv_double_click
          FOR EVENT double_click OF zcl_uitb_alv_events
      IMPORTING
          ev_column
          ev_row.
    "! <p class="shorttext synchronized" lang="en">Handler for ALV User function</p>
    METHODS on_alv_function
          FOR EVENT function_chosen OF zcl_uitb_alv_events
      IMPORTING
          ev_function
          ev_tag.
  PRIVATE SECTION.
    DATA mv_filter_prompt TYPE string.
    METHODS select_rows
      IMPORTING
        if_select TYPE abap_bool.
    "! <p class="shorttext synchronized" lang="en">Accept selected data</p>
    METHODS accept_selections.
ENDCLASS.



CLASS zcl_uitb_selection_dialog IMPLEMENTATION.

  METHOD constructor.
    super->constructor( iv_title ).
    mv_filter_prompt = iv_filter_prompt.
    mf_multi_select = if_multi_select.
    mf_use_alv_filter = if_use_alv_filter.
  ENDMETHOD.

  METHOD zif_uitb_gui_command_handler~execute_command.
    CASE io_command->mv_function.

      WHEN c_func_focus_alv.
        mo_alv->zif_uitb_gui_control~focus( ).

      WHEN c_func_focus_input.
        cl_gui_control=>set_focus( mo_filter_input ).

      WHEN c_func_select_all.
        select_rows( if_select = abap_true ).

      WHEN c_func_unselect_all.
        select_rows( if_select = abap_false ).

      WHEN c_func_accept_selections.
        accept_selections( ).

    ENDCASE.
  ENDMETHOD.

  METHOD create_content.

    DATA(lo_splitter) = NEW zcl_uitb_gui_splitter_cont(
      iv_elements = 2
      iv_size     = '30:*'
      io_parent   = io_container
    ).

    mo_filter_input = NEW cl_gui_input_field(
      parent               = lo_splitter->get_container( 1 )
      input_prompt_text    = COND #( WHEN mv_filter_prompt IS NOT INITIAL THEN mv_filter_prompt ELSE 'Enter Filter' )
      label_text           = 'Filter'
      label_width          = 10
      activate_find_button = abap_true
    ).
    SET HANDLER:
      on_filter_submit FOR mo_filter_input.

    create_alv( lo_splitter->get_container( 2 ) ).
  ENDMETHOD.


  METHOD create_alv.

    FIELD-SYMBOLS: <lt_output_data>   TYPE table,
                   <lt_filtered_data> TYPE table.

    mr_t_data = get_output_table( ).
    ASSERT mr_t_data IS BOUND.

    mo_alv = zcl_uitb_alv=>create_alv(
       ir_data      = mr_t_data
       ir_container = io_container
    ).
    DATA(lo_cols) = mo_alv->get_columns( ).
    DATA(lo_col_iterator) = lo_cols->zif_uitb_list~get_iterator( ).

    WHILE lo_col_iterator->has_next( ).
      DATA(lo_col) = CAST zcl_uitb_alv_column( lo_col_iterator->get_next( ) ).
      IF mf_multi_select = abap_true AND lo_col->get_name( ) = get_mark_field( ).
        lo_col->set_cell_type( zif_uitb_c_alv_cell_types=>checkbox_hotspot ).
        lo_col->set_descriptions( iv_medium = 'Sel.' iv_long = 'Selected' ).
        lo_col->set_optimized( ).
        lo_col->set_style( zif_uitb_c_alv_cell_style=>enabled ).
      ELSEIF mf_use_alv_filter = abap_true AND lo_col->get_name( ) = get_filtered_column( ).
        lo_col->set_technical( ).
      ELSE.
        adjust_column( lo_col ).
      ENDIF.
    ENDWHILE.

    SET HANDLER:
      on_alv_link_click FOR mo_alv->get_events( ).

    IF mf_multi_select = abap_false.
      SET HANDLER:
        on_alv_double_click FOR mo_alv->get_events( ).
    ENDIF.

    lo_cols->set_single_click_sort( ).

    DATA(lo_functions) = mo_alv->get_functions( ).
    lo_functions->set_all( abap_false ).
    lo_functions->set_function( zif_uitb_c_alv_functions=>column_optimze ).

    IF mf_multi_select = abap_true.
      SET HANDLER:
        on_alv_function FOR mo_alv->get_events( ).

      mo_alv->get_selections( )->set_mode( zif_uitb_c_alv_selection=>cell ).

      lo_functions->add_function(
          iv_name    = c_func_accept_selections
          iv_icon    = |{ icon_execute_object }|
          iv_tooltip = |{ TEXT-003 }|
      ).
      lo_functions->add_function(
          iv_type = zcl_uitb_alv_functions=>separator
      ).
      lo_functions->add_function(
          iv_name    = c_func_select_all
          iv_icon    = |{ icon_select_all }|
          iv_tooltip = |{ 'Select all'(004) }|
      ).
      lo_functions->add_function(
          iv_name    = c_func_unselect_all
          iv_icon    = |{ icon_deselect_all }|
          iv_tooltip = |{ 'Unselect All'(005) }|
      ).
    ENDIF.

    mo_alv->display( ).
    mo_alv->zif_uitb_gui_control~focus( ).
    lo_cols->set_optimized( ).

  ENDMETHOD.

  METHOD get_filtered_data.
    RETURN.
  ENDMETHOD.

  METHOD matches_filter.
    RETURN.
  ENDMETHOD.

  METHOD adjust_column.
    RETURN.
  ENDMETHOD.

  METHOD on_filter_submit.
    FIELD-SYMBOLS: <lt_output_data>   TYPE table,
                   <lt_filtered_data> TYPE table.

    ASSIGN mr_t_data->* TO <lt_output_data>.


    IF mf_use_alv_filter = abap_true.
      DATA(lo_filters) = mo_alv->get_filters( ).
      lo_filters->clear( ).
      IF input IS NOT INITIAL.
        TRY.
            lo_filters->add_filter(
                iv_columnname = get_filtered_column( )
                iv_low        = |{ abap_true }|
            ).
          CATCH zcx_uitb_alv_error.
            MESSAGE |Column { get_filtered_column( ) } not found in table| TYPE 'X'.
        ENDTRY.

        LOOP AT <lt_output_data> ASSIGNING FIELD-SYMBOL(<ls_output_data>).
          DATA(lv_index) = sy-tabix.
          ASSIGN COMPONENT get_filtered_column( ) OF STRUCTURE <ls_output_data> TO FIELD-SYMBOL(<lv_filtered>).
          CHECK sy-subrc = 0.

          CLEAR <lv_filtered>.
          <lv_filtered> = matches_filter( iv_filter = input
                                          is_data   = <ls_output_data> ).
        ENDLOOP.

      ENDIF.
    ELSE.
      DATA(lr_t_filtered_data) = get_filtered_data( input ).
      IF lr_t_filtered_data IS BOUND.
        ASSIGN lr_t_filtered_data->* TO <lt_filtered_data>.
        IF  <lt_filtered_data> IS ASSIGNED AND
            <lt_output_data> IS ASSIGNED.
          MOVE-CORRESPONDING <lt_filtered_data> TO <lt_output_data>.
        ENDIF.
      ENDIF.
    ENDIF.

    mo_alv->refresh( ).

  ENDMETHOD.

  METHOD on_alv_link_click.
    set_selected_element( EXPORTING iv_row    = ev_row
                                    iv_column = ev_column ).
    IF mf_multi_select = abap_true.
      mo_alv->refresh( ).
    ELSE.
      leave_screen( ).
    ENDIF.
  ENDMETHOD.

  METHOD on_alv_function.
    CASE ev_function.

      WHEN c_func_accept_selections.
        accept_selections( ).

      WHEN c_func_select_all.
        select_rows( EXPORTING if_select = abap_true ).

      WHEN c_func_unselect_all.
        select_rows( EXPORTING if_select = abap_false ).

    ENDCASE.
  ENDMETHOD.

  METHOD on_alv_double_click.
    set_selected_element( EXPORTING iv_row    = ev_row
                                    iv_column = ev_column ).
    leave_screen( ).
  ENDMETHOD.

  METHOD do_before_dynpro_output.
    DATA: lt_key_map TYPE zif_uitb_ty_gui_screen=>ty_t_fkey_map.

    IF io_callback->is_first_screen_call( ).
      CLEAR mf_data_selected.
    ENDIF.

    IF mf_multi_select = abap_false.
      lt_key_map = VALUE #(
        ( fkey = zif_uitb_c_gui_screen=>c_functions-f6 mapped_function = c_func_focus_alv          text = |{ 'Set focus to ALV'(001) }| )
        ( fkey = zif_uitb_c_gui_screen=>c_functions-f5 mapped_function = c_func_focus_input        text = |{ 'Set focus to filter field'(002) }| )
      ).
    ELSE.
      lt_key_map = VALUE #( BASE lt_key_map
        ( fkey = zif_uitb_c_gui_screen=>c_functions-f5 mapped_function = c_func_select_all         text = |{ TEXT-004 }| )
        ( fkey = zif_uitb_c_gui_screen=>c_functions-f6 mapped_function = c_func_unselect_all       text = |{ TEXT-005 }| )
        ( fkey = zif_uitb_c_gui_screen=>c_functions-f8 mapped_function = c_func_accept_selections  text = |{ 'Accept Selections'(003) }| )
      ).
    ENDIF.

    io_callback->map_fkey_functions( lt_key_map ).
  ENDMETHOD.

  METHOD has_selections.
    RETURN.
  ENDMETHOD.

  METHOD get_filtered_column.
    rv_field = c_filtered_field.
  ENDMETHOD.

  METHOD get_mark_field.
    rv_field = c_mark_default_field.
  ENDMETHOD.


  METHOD select_rows.
    FIELD-SYMBOLS: <lt_data> TYPE table.

    ASSIGN mr_t_data->* TO <lt_data>.

    LOOP AT <lt_data> ASSIGNING FIELD-SYMBOL(<ls_data>).
      DATA(lv_mark_field) = get_mark_field( ).
      CHECK lv_mark_field IS NOT INITIAL.

      ASSIGN COMPONENT lv_mark_field OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_mark>).
      CHECK sy-subrc = 0.

      <lv_mark> = if_select.
    ENDLOOP.

    mo_alv->refresh( ).
  ENDMETHOD.


  METHOD accept_selections.
    IF NOT has_selections( ).
      MESSAGE |Select at least 1 element| TYPE 'I'.
    ELSE.
      mf_data_selected = abap_true.
      leave_screen( ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
