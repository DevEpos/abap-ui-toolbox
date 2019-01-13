class ZCL_UITB_FULLSCREEN_ALV_TABLE definition
  public
  abstract
  create public .

public section.

  methods CLOSE .
  methods SHOW
    importing
      !IF_SHOW_AS_DIALOG type BOOLEAN optional .
  methods SHOW_DOCKED
    importing
      !IV_DOCK_AT type I default CL_GUI_DOCKING_CONTAINER=>DOCK_AT_BOTTOM
      !IV_DOCK_RATIO type I optional .
protected section.

  data MR_SALV_TABLE type ref to CL_SALV_TABLE .
  data MF_SHOW_AS_DIALOG type SAP_BOOL .
  data MF_SHOW_DOCKED type ABAP_BOOL .

  methods SHOW_INTERNAL .
    "! Selection of data for the ALV output
    "! @raising zcx_uitb_alv_no_data | No data is found
  methods SELECT_DATA
  abstract
    raising
      ZCX_UITB_ALV_NO_DATA .
    "! Registers the double_click event on ALV <br/>
    "! Should be called in do_before_alv_creation
  methods REGISTER_DOUBLE_CLICK .
    "! Registers the link_click event on ALV <br/>
    "! Should be called in do_before_alv_creation
  methods REGISTER_LINK_CLICK .
    "! Called directly before ALV refresh
  methods DO_BEFORE_REFRESH .
  methods ON_USER_COMMAND
  abstract
    for event ADDED_FUNCTION of CL_SALV_EVENTS
    importing
      !E_SALV_FUNCTION .
  methods ON_DOUBLE_CLICK
    for event DOUBLE_CLICK of CL_SALV_EVENTS_TABLE
    importing
      !COLUMN
      !ROW .
  methods ON_LINK_CLICK
    for event LINK_CLICK of CL_SALV_EVENTS_TABLE
    importing
      !COLUMN
      !ROW .
  methods REFRESH
    importing
      !IF_REFRESH_FROM_DB type BOOLEAN optional
      !IF_UPDATE_TITLE type BOOLEAN optional .
  methods GET_SELECTED_ROW
    returning
      value(RV_SELECTED_ROW_INDEX) type SY-TABIX .
  methods GET_REPORT_ID
  abstract
    returning
      value(RV_REPID) type SY-REPID .
  methods GET_STATUS
  abstract
    returning
      value(RV_STATUS) type SYPFKEY .
  methods GET_SELECTION_MODE
    returning
      value(RV_SEL_MODE) type I .
  methods GET_TITLE
  abstract
    returning
      value(RV_TITLE) type LVC_TITLE .
  methods GET_TABLE_REFERENCE
  abstract
    returning
      value(RR_TABLE_REF) type ref to DATA .
  methods ADJUST_COLUMNS
    importing
      !IR_COLUMNS type ref to CL_SALV_COLUMNS_TABLE
    raising
      CX_SALV_ERROR .
  methods ADJUST_DISPLAY_SETTINGS
    importing
      !IR_DISPLAY_SETTINGS type ref to CL_SALV_DISPLAY_SETTINGS
    raising
      CX_SALV_ERROR .
  methods ADJUST_FUNCTIONS
    importing
      !IR_FUNCTIONS type ref to CL_SALV_FUNCTIONS_LIST .
    "! Returns the popup dimensions for the ALV dialog
    "! @parameter ev_start_column | (default = 10 )
    "! @parameter ev_end_column   | (default = 140)
    "! @parameter ev_start_line   | (default = 2  )
    "! @parameter ev_end_line     | (default = 25 )
  methods GET_POPUP_DIMENSIONS
    exporting
      !EV_START_COLUMN type I
      !EV_END_COLUMN type I
      !EV_START_LINE type I
      !EV_END_LINE type I .
  methods ENRICH_DATA .
  methods GET_SELECTED_ROWS
    returning
      value(RT_SELECTED_ROWS) type SALV_T_ROW .
  methods DO_BEFORE_ALV_CREATION .
  methods DO_AFTER_ALV_CREATION .
private section.

  data MV_DOCK_AT type I .
  data MV_DOCK_RATIO type I .
  data MF_DO_REGISTER_DOUBLE_CLICK type SAP_BOOL .
  data MF_DO_REGISTER_LINK_CLICK type SAP_BOOL .
  data MR_TABLE_DATA type ref to DATA .
  data MR_DOCK type ref to CL_GUI_DOCKING_CONTAINER .

  methods INITIALIZE_OUTPUT_TABLE .
  methods SET_DIALOG_DIMENSIONS .
  methods CREATE_ALV .
  methods DISPLAY_DATA .
  methods REGISTER_EVENT_HANDLERS
    importing
      !IR_EVENTS type ref to CL_SALV_EVENTS_TABLE .
  methods CREATE_SALV_OBJECT
    raising
      CX_SALV_ERROR .
  methods CREATE_DOCKING_CONTAINER
    returning
      value(RR_CONTAINER) type ref to CL_GUI_CONTAINER .
ENDCLASS.



CLASS ZCL_UITB_FULLSCREEN_ALV_TABLE IMPLEMENTATION.


  METHOD adjust_columns ##needed.
  ENDMETHOD.


  METHOD adjust_display_settings ##needed.
  ENDMETHOD.


  METHOD adjust_functions.
    ir_functions->set_all( ).
  ENDMETHOD.


  METHOD close.
    CHECK mr_salv_table IS NOT INITIAL.

    IF mr_dock IS NOT INITIAL.
      mr_dock->free( ).
      CLEAR mr_dock.
    ELSE.
      mr_salv_table->close_screen( ).
    ENDIF.
  ENDMETHOD.


  METHOD create_alv.
    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    ASSIGN mr_table_data->* TO <lt_table>.

    TRY .
        create_salv_object( ).

        adjust_columns( mr_salv_table->get_columns( ) ).
        adjust_display_settings( mr_salv_table->get_display_settings( ) ).
        mr_salv_table->get_selections( )->set_selection_mode( get_selection_mode( ) ).
        mr_salv_table->get_display_settings( )->set_list_header( get_title( ) ).
        adjust_functions( mr_salv_table->get_functions( ) ).

        IF mf_show_docked = abap_false.
          mr_salv_table->set_screen_status(
            report   = get_report_id( )
            pfstatus = get_status( )
          ).
        ENDIF.

        IF mf_show_as_dialog = abap_true.
          set_dialog_dimensions( ).
        ENDIF.

        register_event_handlers( mr_salv_table->get_event( ) ).

      CATCH cx_salv_error INTO DATA(lr_salv_error) .
        MESSAGE lr_salv_error TYPE 'E'.
        RETURN.
    ENDTRY.
  ENDMETHOD.


  METHOD create_docking_container.
    mr_dock = NEW cl_gui_docking_container( side   = mv_dock_at
                                            ratio  = mv_dock_ratio ).
    rr_container = mr_dock.
  ENDMETHOD.


  METHOD create_salv_object.
    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    ASSIGN mr_table_data->* TO <lt_table>.

    IF mf_show_docked = abap_true.
      cl_salv_table=>factory(
        EXPORTING
          r_container    = create_docking_container( )
        IMPORTING
          r_salv_table   = mr_salv_table
        CHANGING
          t_table        = <lt_table>
      ).
    ELSE.

      cl_salv_table=>factory(
        IMPORTING
          r_salv_table   = mr_salv_table
        CHANGING
          t_table        = <lt_table>
      ).
    ENDIF.
  ENDMETHOD.


  METHOD display_data.
    mr_salv_table->get_columns( )->set_optimize( ).
    mr_salv_table->display( ).
  ENDMETHOD.


  METHOD do_after_alv_creation.
    RETURN.
  ENDMETHOD.


  METHOD do_before_alv_creation.
    RETURN.
  ENDMETHOD.


  METHOD do_before_refresh.
    RETURN.
  ENDMETHOD.


  METHOD enrich_data.
    RETURN.
  ENDMETHOD.


  METHOD get_popup_dimensions.
    ev_start_column = 10.
    ev_end_column   = 140.
    ev_start_line   = 2.
    ev_end_line     = 25.
  ENDMETHOD.


  METHOD get_selected_row.
    " get selected line
    DATA(lt_selected_rows) = get_selected_rows( ).

    IF lt_selected_rows IS NOT INITIAL AND
       lines( lt_selected_rows ) = 1.
      rv_selected_row_index = lt_selected_rows[ 1 ].
    ENDIF.
  ENDMETHOD.


  METHOD get_selected_rows.
    " get selected line
    DATA(lt_selected_rows) =  mr_salv_table->get_selections( )->get_selected_rows( ).
    IF lt_selected_rows IS INITIAL.
      DATA(lt_selected_cells) = mr_salv_table->get_selections( )->get_selected_cells( ).
      IF lt_selected_cells IS INITIAL.
        RETURN.
      ELSE. " condense cells by row
        SORT lt_selected_cells BY row.
        DELETE ADJACENT DUPLICATES FROM lt_selected_cells COMPARING row.
        lt_selected_rows = VALUE #( FOR cell IN lt_selected_cells ( cell-row ) ).
      ENDIF.
    ENDIF.

    rt_selected_rows = lt_selected_rows.

  ENDMETHOD.


  METHOD get_selection_mode.
    rv_sel_mode = if_salv_c_selection_mode=>none.
  ENDMETHOD.


  METHOD initialize_output_table.
    mr_table_data = get_table_reference( ).
  ENDMETHOD.


  METHOD on_double_click.
    RETURN.
  ENDMETHOD.


  METHOD on_link_click.
    RETURN.
  ENDMETHOD.


  METHOD refresh.
    TRY.
        IF if_refresh_from_db = abap_true.
          select_data( ).
          enrich_data( ).
        ENDIF.

        IF if_update_title = abap_true.
          mr_salv_table->get_display_settings( )->set_list_header( get_title( ) ).
        ENDIF.

        mr_salv_table->refresh(
            s_stable     = VALUE #( row = abap_true col = abap_true )
        ).

      CATCH zcx_uitb_alv_error INTO DATA(lr_exc).
        lr_exc->show_message( ).
    ENDTRY.
  ENDMETHOD.


  METHOD register_double_click.
    mf_do_register_double_click = abap_true.
  ENDMETHOD.


  METHOD register_event_handlers.
    SET HANDLER on_user_command FOR ir_events.

    IF mf_do_register_double_click = abap_true.
      SET HANDLER on_double_click FOR ir_events.
    ENDIF.

    IF mf_do_register_link_click = abap_true.
      SET HANDLER on_link_click FOR ir_events.
    ENDIF.
  ENDMETHOD.


  METHOD register_link_click.
    mf_do_register_link_click = abap_true.
  ENDMETHOD.


  METHOD set_dialog_dimensions.
    get_popup_dimensions(
      IMPORTING
        ev_start_column = DATA(lv_start_column)
        ev_end_column   = DATA(lv_end_column)
        ev_start_line   = DATA(lv_start_line)
        ev_end_line     = DATA(lv_end_line)
    ).
    mr_salv_table->set_screen_popup(
        start_column = lv_start_column
        end_column   = lv_end_column
        start_line   = lv_start_line
        end_line     = lv_end_line
    ).
  ENDMETHOD.


  METHOD show.
    mf_show_as_dialog = if_show_as_dialog.
    show_internal( ).
  ENDMETHOD.


  METHOD show_docked.
    mf_show_docked = abap_true.
    mv_dock_at = iv_dock_at.
    mv_dock_ratio = iv_dock_ratio.
    show_internal( ).
  ENDMETHOD.


  METHOD show_internal.
    TRY.
        initialize_output_table( ).

        select_data( ).

        enrich_data( ).

        do_before_alv_creation( ).

        create_alv( ).

        do_after_alv_creation( ).

        display_data( ).
      CATCH zcx_uitb_alv_error INTO DATA(lr_exc).
        lr_exc->show_message( ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
