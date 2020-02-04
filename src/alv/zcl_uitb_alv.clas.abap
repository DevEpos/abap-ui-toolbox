"! <p class="shorttext synchronized" lang="en">GUI ALV Grid (OO)</p>
CLASS zcl_uitb_alv DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE

  GLOBAL FRIENDS zcl_uitb_alv_grid_adapter
                 zcl_uitb_alv_metadata_util .

  PUBLIC SECTION.

    INTERFACES zif_uitb_gui_control .

    "! <p class="shorttext synchronized" lang="en">Creates new ALV instance</p>
    "!
    CLASS-METHODS create_alv
      IMPORTING
        !ir_data                 TYPE REF TO data
        iv_display_type          TYPE i DEFAULT zif_uitb_c_alv_display_types=>embedded
        !iv_description_language TYPE langu DEFAULT sy-langu
        !ir_container            TYPE REF TO cl_gui_container OPTIONAL
        !if_editable             TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rr_alv)            TYPE REF TO zcl_uitb_alv .
    "! <p class="shorttext synchronized" lang="en">Get current data of alv</p>
    "!
    METHODS get_data
      RETURNING
        VALUE(rr_data) TYPE REF TO data .
    "! <p class="shorttext synchronized" lang="en">Trigger given function in ALV</p>
    "!
    METHODS set_function
      IMPORTING
        !iv_function TYPE ui_func .
    "! <p class="shorttext synchronized" lang="en">Set dimensions for popup</p>
    "!
    METHODS set_popup_dimensions
      IMPORTING
        iv_top    TYPE i
        iv_left   TYPE i
        iv_right  TYPE i
        iv_bottom TYPE i.
    METHODS get_metadata .
    METHODS get_filters
      RETURNING
        VALUE(result) TYPE REF TO zcl_uitb_alv_filters .
    METHODS set_sorting
      IMPORTING
        !it_sorting TYPE lvc_t_sort .
    METHODS set_editable
      IMPORTING
        !value TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Stores the given user data in this ALV</p>
    METHODS store_user_data
      IMPORTING
        ir_user_data TYPE REF TO data.
    "! <p class="shorttext synchronized" lang="en">Returns stored user data if there is any</p>
    METHODS get_user_data
      RETURNING
        VALUE(rr_user_data) TYPE REF TO data.
    "! <p class="shorttext synchronized" lang="en">Closes the ALV</p>
    "! <strong>NOTE:</strong> The alv has to be opened in dialog mode
    "! for this to work
    METHODS close.
    METHODS display
      RAISING
        zcx_uitb_alv_error .
    METHODS get_columns
      RETURNING
        VALUE(result) TYPE REF TO zcl_uitb_alv_columns .
    METHODS register_edit_for_modified
      IMPORTING
        !value TYPE abap_bool DEFAULT abap_true .
    METHODS refresh
      IMPORTING
        !if_soft                 TYPE abap_bool OPTIONAL
        !is_stable               TYPE lvc_s_stbl OPTIONAL
        !if_keep_scroll_position TYPE abap_bool OPTIONAL .
    METHODS get_sorting
      RETURNING
        VALUE(result) TYPE REF TO zcl_uitb_alv_sorts .
    "! <p class="shorttext synchronized" lang="en">Retrieve Layout for ALV</p>
    METHODS get_layout
      RETURNING
        VALUE(result) TYPE REF TO zcl_uitb_alv_layout .
    METHODS get_events
      RETURNING
        VALUE(result) TYPE REF TO zcl_uitb_alv_events .
    METHODS get_functions
      RETURNING
        VALUE(result) TYPE REF TO zcl_uitb_alv_functions .
    METHODS get_selections
      RETURNING
        VALUE(result) TYPE REF TO zcl_uitb_alv_selections .
    METHODS get_data_changes
      RETURNING
        VALUE(result) TYPE REF TO zcl_uitb_alv_data .
    METHODS get_functional_settings
      RETURNING
        VALUE(result) TYPE REF TO zcl_uitb_alv_func_settings .
    METHODS get_display_settings
      RETURNING
        VALUE(result) TYPE REF TO zcl_uitb_alv_display_settings .
    METHODS set_data
      IMPORTING
        !ir_data TYPE REF TO data .
    "! <p class="shorttext synchronized" lang="en">Set visibility of Grid control</p>
    METHODS set_visible
      IMPORTING
        value TYPE abap_bool DEFAULT abap_true.
  PROTECTED SECTION.

    "! <p class="shorttext synchronized" lang="en">Perform Quick filter</p>
    METHODS perform_quick_filter
      IMPORTING
        !if_exclude TYPE abap_bool OPTIONAL .
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_change_character,
        sorting TYPE char1 VALUE 'S',
        layout  TYPE char1 VALUE 'L',
        columns TYPE char1 VALUE 'C',
      END OF c_change_character .
    CONSTANTS c_delete_row_custom TYPE string VALUE 'DELETE_ROW' ##NO_TEXT.
    DATA:
      mt_changes TYPE HASHED TABLE OF char1 WITH UNIQUE KEY table_line .
    DATA mo_controller TYPE REF TO zcl_uitb_alv_controller .
    DATA mo_alv_data_wrapper TYPE REF TO zcl_uitb_alv_data .
    DATA mo_functions TYPE REF TO zcl_uitb_alv_functions .
    DATA mo_functional_settings TYPE REF TO zcl_uitb_alv_func_settings .
    DATA mo_filters TYPE REF TO zcl_uitb_alv_filters .
    DATA mo_display_settings TYPE REF TO zcl_uitb_alv_display_settings .
    DATA mr_data TYPE REF TO data .
    DATA mo_columns TYPE REF TO zcl_uitb_alv_columns .
    DATA mo_events TYPE REF TO zcl_uitb_alv_events .
    DATA mt_exclude_toolbar TYPE ui_functions .
    DATA mf_register_modified TYPE abap_bool .
    DATA mo_selections TYPE REF TO zcl_uitb_alv_selections .
    DATA mt_sorting TYPE lvc_t_sort .
    DATA mr_container TYPE REF TO cl_gui_container .
    DATA mr_sorting TYPE REF TO zcl_uitb_alv_sorts .
    "! <p class="shorttext synchronized" lang="en">Layout for ALV Grid</p>
    DATA mo_layout TYPE REF TO zcl_uitb_alv_layout .
    DATA:
      BEGIN OF ms_popup_dimensions,
        top    TYPE i,
        left   TYPE i,
        right  TYPE i,
        bottom TYPE i,
        width  TYPE i,
        height TYPE i,
      END OF ms_popup_dimensions.
    DATA mv_display_type TYPE i.
    DATA: mr_user_data TYPE REF TO data.
    METHODS init
      IMPORTING
        !if_editable             TYPE abap_bool
        !iv_description_language TYPE langu .
    METHODS constructor
      IMPORTING
        !ir_data        TYPE REF TO data
        iv_display_type TYPE i
        !ir_container   TYPE REF TO cl_gui_container .
ENDCLASS.



CLASS zcl_uitb_alv IMPLEMENTATION.


  METHOD constructor.
    mr_data = ir_data.
    mr_container = ir_container.
    mv_display_type = iv_display_type.
  ENDMETHOD.


  METHOD create_alv.
    rr_alv = NEW #(
      ir_data         = ir_data
      iv_display_type = iv_display_type
      ir_container    = ir_container
    ).

    rr_alv->init(
        if_editable             = if_editable
        iv_description_language = iv_description_language
    ).
  ENDMETHOD.

  METHOD store_user_data.
    mr_user_data = ir_user_data.
  ENDMETHOD.

  METHOD get_user_data.
    rr_user_data = mr_user_data.
  ENDMETHOD.

  METHOD close.
    mo_controller->mo_adapter->close( ).
  ENDMETHOD.

  METHOD display.
    CHECK mo_controller IS BOUND.

    mo_controller->display( ).

  ENDMETHOD.


  METHOD get_columns.
    IF mo_columns IS INITIAL.
      mo_columns = NEW #(
        ir_data       = mr_data
        ir_controller = mo_controller
      ).
    ENDIF.

    result = mo_columns.
  ENDMETHOD.

  METHOD set_visible.
    IF mo_controller IS BOUND.
      DATA(lo_grid) = mo_controller->mo_adapter->get_grid( ).
      IF lo_grid IS BOUND.
        lo_grid->set_visible( COND #( WHEN value = abap_true THEN cl_gui_control=>visible_true ELSE cl_gui_control=>visible_false ) ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_data.
    rr_data = mr_data.
  ENDMETHOD.


  METHOD get_data_changes.
    IF mo_alv_data_wrapper IS NOT BOUND.
      mo_alv_data_wrapper = NEW #(
        ir_controller = mo_controller
        ir_data       = mr_data
      ).
    ENDIF.

    result = mo_alv_data_wrapper.
  ENDMETHOD.


  METHOD get_display_settings.
    IF mo_display_settings IS NOT BOUND.
      mo_display_settings = NEW #( io_controller = mo_controller ).
    ENDIF.

    result = mo_display_settings.
  ENDMETHOD.


  METHOD get_events.
    IF mo_events IS NOT BOUND.
      mo_events = NEW #( io_controller = mo_controller ).
    ENDIF.

    result = mo_events.
  ENDMETHOD.


  METHOD get_filters.
    IF mo_filters IS INITIAL.
      mo_filters = NEW zcl_uitb_alv_filters(
        ir_controller = mo_controller
        ir_columns    = get_columns( )
      ).
    ENDIF.

    result = mo_filters.
  ENDMETHOD.


  METHOD get_functional_settings.
    IF mo_functional_settings IS NOT BOUND.
      mo_functional_settings = NEW #( ir_controller = mo_controller ).
    ENDIF.

    result = mo_functional_settings.
  ENDMETHOD.


  METHOD get_functions.
    IF mo_functions IS INITIAL.
      mo_functions = NEW #( mo_controller ).
    ENDIF.

    result = mo_functions.
  ENDMETHOD.


  METHOD get_layout.
    IF mo_layout IS INITIAL.
      mo_layout = NEW #( ir_controller = mo_controller ).
    ENDIF.

    result = mo_layout.
  ENDMETHOD.


  METHOD get_metadata.
    DATA(lo_adapter) = mo_controller->mo_adapter.
    IF lo_adapter IS BOUND AND NOT lo_adapter->is_function_call_active( ).
      lo_adapter->get_metadata( ).
    ENDIF.
  ENDMETHOD.


  METHOD get_selections.
    IF mo_selections IS INITIAL.
      mo_selections = NEW #( ir_controller = mo_controller ).
    ENDIF.

    result = mo_selections.
  ENDMETHOD.


  METHOD get_sorting.
    IF mr_sorting IS INITIAL.
      mr_sorting = NEW #( ir_controller = mo_controller
                          ir_columns    = get_columns( ) ).
    ENDIF.

    result = mr_sorting.
  ENDMETHOD.


  METHOD init.

    " create the controller
    mo_controller = NEW #( io_model = me ).

    get_columns( )->set_description_language( iv_description_language ).
    get_functional_settings( ).
    get_events( ).
    get_display_settings( ).
    get_functions( ).
    get_data_changes( ).

    mo_display_settings->set_editable( if_editable ).

    " create columns from input data
    set_data( mr_data ).
  ENDMETHOD.


  METHOD perform_quick_filter.
    TYPES:
      BEGIN OF lty_filter,
        column TYPE fieldname,
        selopt TYPE zuitb_generic_range_itab,
      END OF lty_filter.

    TYPES: ltt_filter TYPE SORTED TABLE OF lty_filter WITH UNIQUE KEY column.

    DATA: lo_filter TYPE REF TO zcl_uitb_alv_filter.

    FIELD-SYMBOLS: <lt_table> TYPE table.

    DATA(lo_filters) = get_filters( ).
    DATA(lo_columns) = get_columns( ).
    DATA(lt_selected_cells) = get_selections( )->get_selected_cells( ).

    SORT lt_selected_cells BY row.

    ASSIGN mr_data->* TO <lt_table>.

    LOOP AT lt_selected_cells ASSIGNING FIELD-SYMBOL(<ls_cell>).

      AT NEW row.
        ASSIGN <lt_table>[ <ls_cell>-row ] TO FIELD-SYMBOL(<ls_line>).
        CHECK sy-subrc = 0.
      ENDAT.

      TRY.
          DATA(lo_column) = lo_columns->get_column( <ls_cell>-column ).
        CATCH zcx_uitb_alv_not_found.
          CONTINUE.
      ENDTRY.

      ASSIGN COMPONENT <ls_cell>-column OF STRUCTURE <ls_line> TO FIELD-SYMBOL(<lv_value>).
      TRY.
          lo_filter = lo_filters->get_filter( <ls_cell>-column ).
        CATCH zcx_uitb_alv_not_found.
          lo_filter = lo_filters->add_filter( iv_columnname = <ls_cell>-column ).
      ENDTRY.

      CHECK lo_filter IS BOUND.

      lo_filter->add_selopt(
          iv_sign   = COND #( WHEN if_exclude = abap_true THEN 'E' ELSE 'I' )
          iv_option = 'EQ'
          iv_low    = |{ <lv_value> }|
      ).
    ENDLOOP.
  ENDMETHOD.


  METHOD refresh.
    CHECK mo_controller IS BOUND.

    DATA(lv_refresh_mode) = COND #( WHEN if_soft = abap_true THEN
                                      zif_uitb_c_alv_refresh=>soft
                                    ELSE
                                      zif_uitb_c_alv_refresh=>full ).

    mo_controller->zif_uitb_alv_metadata_ctrller~set_changed(
        iv_name         = 'ZCL_UITB_EDITABLE_ALV'
        iv_flavour      = zif_uitb_c_alv_chglist_flavor=>refresh
        iv_refresh_mode = lv_refresh_mode
        iv_method       = 'REFRESH'
    ).

    IF is_stable IS SUPPLIED.
      mo_controller->refresh(
          is_stable               = is_stable
          if_keep_scroll_position = if_keep_scroll_position
      ).
    ELSE.
      mo_controller->refresh(
          is_stable               = VALUE #( row = abap_true col = abap_true )
          if_keep_scroll_position = if_keep_scroll_position
      ).
    ENDIF.
  ENDMETHOD.


  METHOD register_edit_for_modified.
    mf_register_modified = value.
  ENDMETHOD.


  METHOD set_data.

*.. @TODO: Check if new data is allowed
    CHECK ir_data IS NOT INITIAL.

    DATA(lo_columns) = get_columns( ).
    mr_data = ir_data.

    zcl_uitb_alv_data_descr=>describe_table(
        io_columns = lo_columns
        ir_table   = mr_data
    ).
    get_filters( )->clear( ).

    CHECK mo_controller IS BOUND.

    mo_controller->zif_uitb_alv_metadata_ctrller~set_changed(
        iv_name         = 'ZCL_UITB_EDITABLE_ALV'
        iv_flavour      = zif_uitb_c_alv_chglist_flavor=>data_set
        iv_method       = 'SET_DATA'
    ).

  ENDMETHOD.


  METHOD set_editable.
    " @TODO: maybe this is needed if grid can be switched into read only mode
  ENDMETHOD.

  METHOD set_popup_dimensions.
    ms_popup_dimensions = VALUE #(
      top    = iv_top
      left   = iv_left
      right  = iv_right
      width  = iv_right - iv_left
      height = iv_bottom - iv_top
    ).
  ENDMETHOD.

  METHOD set_function.
    CHECK mo_controller IS BOUND.

    mo_controller->set_function( iv_function ).
  ENDMETHOD.


  METHOD set_sorting.
    mt_sorting = it_sorting.
    INSERT c_change_character-sorting INTO TABLE mt_changes.
  ENDMETHOD.

  METHOD zif_uitb_gui_control~has_focus.
    RETURN.
  ENDMETHOD.

  METHOD zif_uitb_gui_control~focus.
    CHECK mo_controller IS BOUND.
    mo_controller->focus( ).
  ENDMETHOD.
ENDCLASS.
