CLASS zcl_uitb_alv_grid_adapter DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  GLOBAL FRIENDS zif_uitb_alv_controller.

  PUBLIC SECTION.
    INTERFACES zif_uitb_alv_adpt_selections.
    METHODS constructor
      IMPORTING
        ir_controller TYPE REF TO zcl_uitb_alv_controller.
    METHODS get_metadata.
    METHODS set_function
      IMPORTING
        iv_function TYPE ui_func.
    METHODS set_metadata
      IMPORTING
        is_stable               TYPE lvc_s_stbl OPTIONAL
        if_keep_scroll_position TYPE abap_bool OPTIONAL
        ir_changelist           TYPE REF TO zcl_uitb_alv_changelist
      RAISING
        zcx_uitb_alv_error.
    METHODS get_grid
      RETURNING
        VALUE(result) TYPE REF TO cl_gui_alv_grid.
    METHODS is_function_call_active
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS set_function_call_active
      IMPORTING
        value TYPE abap_bool DEFAULT abap_true.
    METHODS set_focus_to_grid.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mr_grid TYPE REF TO cl_gui_alv_grid.
    DATA ms_stable TYPE lvc_s_stbl.
    DATA mf_keep_scroll_position TYPE abap_bool.
    DATA mr_controller TYPE REF TO zcl_uitb_alv_controller.
    DATA mf_function_call_active TYPE abap_bool.
    DATA mr_quickfilter_menu TYPE REF TO cl_ctmenu.

    METHODS get_function_tag
      IMPORTING
        iv_user_function TYPE ui_func
      RETURNING
        VALUE(result)    TYPE string.
    METHODS on_user_command
          FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING
          !e_ucomm .
    METHODS on_before_user_command
          FOR EVENT before_user_command OF cl_gui_alv_grid
      IMPORTING
          !e_ucomm .
    METHODS on_after_user_command
          FOR EVENT after_user_command OF cl_gui_alv_grid
      IMPORTING
          !e_not_processed
          !e_saved
          !e_ucomm .
    METHODS on_toolbar
          FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING
          !e_interactive
          !e_object .
    METHODS on_hotspot_click
          FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING
          es_row_no
          e_column_id
          e_row_id.
    METHODS on_button_click
          FOR EVENT button_click OF cl_gui_alv_grid
      IMPORTING
          es_col_id
          es_row_no.
    METHODS on_double_click
          FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING
          es_row_no
          e_column
          e_row.
    METHODS on_context_menu
          FOR EVENT context_menu_request OF cl_gui_alv_grid
      IMPORTING
          e_object.
    METHODS on_menu_button
          FOR EVENT menu_button OF cl_gui_alv_grid
      IMPORTING
          e_object
          e_ucomm.
    METHODS on_f4
          FOR EVENT onf4 OF cl_gui_alv_grid
      IMPORTING
          e_fieldname
          e_fieldvalue
          es_row_no
          er_event_data
          et_bad_cells
          e_display.
    METHODS on_data_changed
          FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING
          er_data_changed
          e_onf4
          e_onf4_before
          e_onf4_after
          e_ucomm.
    METHODS set_event_handlers.
    METHODS on_drag
          FOR EVENT ondrag OF cl_gui_alv_grid
      IMPORTING
          es_row_no
          e_column
          e_dragdropobj
          e_row.
    METHODS on_drop
          FOR EVENT ondrop OF cl_gui_alv_grid
      IMPORTING
          es_row_no
          e_column
          e_dragdropobj
          e_row.
    METHODS on_drop_complete
          FOR EVENT ondropcomplete OF cl_gui_alv_grid
      IMPORTING
          es_row_no
          e_column
          e_dragdropobj
          e_row.
    METHODS on_drop_get_flavor
          FOR EVENT ondropgetflavor OF cl_gui_alv_grid
      IMPORTING
          es_row_no
          e_column
          e_dragdropobj
          e_flavors
          e_row.
ENDCLASS.



CLASS ZCL_UITB_ALV_GRID_ADAPTER IMPLEMENTATION.


  METHOD constructor.
    mr_controller = ir_controller.
    mr_quickfilter_menu = NEW cl_ctmenu( ).
    mr_quickfilter_menu->add_function(
        fcode = zif_uitb_c_alv_functions=>quickfilter_exclude
        text  = 'Exclude value'
    ).
  ENDMETHOD.


  METHOD get_function_tag.
    DATA(lr_functions) = mr_controller->mr_model->get_functions( ).

    result = VALUE #( lr_functions->mt_function_tag_map[ function = iv_user_function ]-tag OPTIONAL ).
  ENDMETHOD.


  METHOD get_grid.
    result = mr_grid.
  ENDMETHOD.


  METHOD get_metadata.
    CHECK mr_grid IS BOUND.

    DATA(lr_model) = mr_controller->mr_model.

    DATA(lr_columns) = lr_model->get_columns( ).
    DATA(lr_functional_settings) = lr_model->get_functional_settings( ).
    DATA(lr_display_settings) = lr_model->get_display_settings( ).
    DATA(lr_selections) = lr_model->get_selections( ).
    DATA(lr_filters) = lr_model->get_filters( ).
    DATA(lr_sorting) = lr_model->get_sorting( ).
    DATA(lr_layout) = lr_model->get_layout( ).

    mr_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(lt_fcat) ).
    mr_grid->get_frontend_layout( IMPORTING es_layout = DATA(ls_layout) ).
    mr_grid->get_filter_criteria( IMPORTING et_filter = DATA(lt_filter) ).
    mr_grid->get_sort_criteria( IMPORTING et_sort = DATA(lt_sort) ).
    mr_grid->get_variant( IMPORTING es_variant = DATA(ls_variant) ).

    lr_selections->invalidate_selections( ).

    zcl_uitb_alv_metadata_util=>get_alv_layout(
        is_layout           = ls_layout
        ir_selections       = lr_selections
        ir_display_settings = lr_display_settings
        ir_columns          = lr_columns
    ).

    zcl_uitb_alv_metadata_util=>get_fieldcatalog(
        it_fieldcat = lt_fcat
        ir_columns  = lr_columns
    ).

    zcl_uitb_alv_metadata_util=>get_filters(
        ir_filters = lr_filters
        it_filter  = lt_filter
    ).

    zcl_uitb_alv_metadata_util=>get_sorting(
        ir_sorts = lr_sorting
        it_sorts = lt_sort
    ).

    zcl_uitb_alv_metadata_util=>set_variant(
        is_variant = ls_variant
        ir_layout  = lr_layout
    ).

  ENDMETHOD.


  METHOD is_function_call_active.
    result = mf_function_call_active.
  ENDMETHOD.


  METHOD on_after_user_command.
    set_function_call_active( ).

    zcl_uitb_alv_evt_controller=>raise_after_function(
        ir_controller = mr_controller
        iv_function   = e_ucomm
    ).

    set_function_call_active( abap_false ).
  ENDMETHOD.


  METHOD on_before_user_command.
    set_function_call_active( ).

    zcl_uitb_alv_evt_controller=>raise_before_function(
        ir_controller = mr_controller
        iv_function   = e_ucomm
    ).

    set_function_call_active( abap_false ).
  ENDMETHOD.


  METHOD on_button_click.
    set_function_call_active( ).

    zcl_uitb_alv_evt_controller=>raise_link_click(
        ir_controller = mr_controller
        iv_column     = es_col_id-fieldname
        iv_row        = es_row_no-row_id
    ).

    set_function_call_active( abap_false ).
  ENDMETHOD.


  METHOD on_context_menu.
    DATA(lr_model) = mr_controller->mr_model.

    DATA(lr_functions) = lr_model->get_functions( ).
    IF lr_functions IS BOUND.
      DATA(lt_disabled) = lr_functions->get_disabled( ).
      e_object->hide_functions( lt_disabled ).

      IF lr_functions->mf_quickfilter = abap_true.
*...... Look for Filter entry
      ENDIF.
    ENDIF.


    set_function_call_active( ).

    zcl_uitb_alv_evt_controller=>raise_context_menu(
        ir_controller = mr_controller
        ir_menu       = e_object
    ).

    set_function_call_active( abap_false ).

  ENDMETHOD.


  METHOD on_data_changed.
    set_function_call_active( ).

    zcl_uitb_alv_evt_controller=>raise_data_changed(
        ir_controller      = mr_controller
        ir_change_protocol = er_data_changed
        iv_function        = e_ucomm
        if_onf4            = e_onf4
        if_onf4_before     = e_onf4_before
        if_onf4_after      = e_onf4_after
    ).

    set_function_call_active( abap_false ).
  ENDMETHOD.


  METHOD on_double_click.
    set_function_call_active( abap_true ).

    zcl_uitb_alv_evt_controller=>raise_double_click(
        ir_controller = mr_controller
        iv_column     = e_column-fieldname
        iv_row        = es_row_no-row_id
    ).

    set_function_call_active( abap_false ).
  ENDMETHOD.


  METHOD on_drag.
    zcl_uitb_alv_evt_controller=>raise_drag(
        ir_controller  = mr_controller
        iv_row         = es_row_no-row_id
        iv_column      = e_column-fieldname
        is_row_no      = es_row_no
        ir_dragdropobj = e_dragdropobj
    ).
  ENDMETHOD.


  METHOD on_drop.
    zcl_uitb_alv_evt_controller=>raise_drop(
        ir_controller  = mr_controller
        iv_row         = es_row_no-row_id
        iv_column      = e_column-fieldname
        is_row_no      = es_row_no
        ir_dragdropobj = e_dragdropobj
    ).
  ENDMETHOD.


  METHOD on_drop_complete.
    zcl_uitb_alv_evt_controller=>raise_drop_complete(
        ir_controller  = mr_controller
        iv_row         = es_row_no-row_id
        iv_column      = e_column-fieldname
        is_row_no      = es_row_no
        ir_dragdropobj = e_dragdropobj
    ).
  ENDMETHOD.


  METHOD on_drop_get_flavor.
    zcl_uitb_alv_evt_controller=>raise_drop_get_flavor(
        ir_controller  = mr_controller
        iv_row         = es_row_no-row_id
        iv_column      = e_column-fieldname
        is_row_no      = es_row_no
        ir_dragdropobj = e_dragdropobj
        it_flavors     = e_flavors
    ).
  ENDMETHOD.


  METHOD on_f4.
    set_function_call_active( ).

    zcl_uitb_alv_evt_controller=>raise_f4(
        ir_controller = mr_controller
        iv_fieldname  = e_fieldname
        iv_fieldvalue = e_fieldvalue
        is_row_no     = es_row_no
        ir_event_data = er_event_data
        it_bad_cells  = et_bad_cells
        if_display    = e_display
    ).

    set_function_call_active( abap_false ).
  ENDMETHOD.


  METHOD on_hotspot_click.
    set_function_call_active( ).

    zcl_uitb_alv_evt_controller=>raise_link_click(
        ir_controller = mr_controller
        iv_column     = e_column_id-fieldname
        iv_row        = es_row_no-row_id
    ).

    set_function_call_active( abap_false ).
  ENDMETHOD.


  METHOD on_menu_button.
  ENDMETHOD.


  METHOD on_toolbar.
    DATA: lt_disabled_selopt TYPE RANGE OF ui_func.

    DATA(lr_model) = mr_controller->mr_model.

    DATA(lr_functions) = lr_model->get_functions( ).
    IF lr_functions IS NOT BOUND.
      RETURN.
    ENDIF.

    DATA(lt_disabled) = lr_functions->get_disabled( ).

    IF lt_disabled IS NOT INITIAL.
      lt_disabled_selopt = VALUE #(
          LET i = 'I' eq = 'EQ' IN
          FOR <lv_func> IN lt_disabled
          ( sign = i
            option = eq
            low = <lv_func> )
      ).

      DELETE e_object->mt_toolbar WHERE function IN lt_disabled_selopt.
    ENDIF.

*.. Insert toolbar functions at start
    IF lr_functions->mt_buttons_front IS NOT INITIAL.
      LOOP AT lr_functions->mt_buttons_front ASSIGNING FIELD-SYMBOL(<ls_button_front>).
        INSERT CORRESPONDING #( <ls_button_front> ) INTO e_object->mt_toolbar INDEX 1.
      ENDLOOP.
    ENDIF.

*.. append toolbar buttons at the end
    e_object->mt_toolbar = VALUE #( BASE e_object->mt_toolbar
      ( LINES OF CORRESPONDING #( lr_functions->mt_buttons ) )
    ).

    IF lr_functions->mt_menus is not INITIAL.
      e_object->mt_btnmnu = value #(
        base e_object->mt_btnmnu
        ( lines of CORRESPONDING #( lr_functions->mt_menus ) )
      ).
    ENDIF.

*.. add quickfilter if set inside functions
    IF lr_functions->mf_quickfilter = abap_true.

      " try to find filter function
      DATA(lv_index) = line_index( e_object->mt_toolbar[ function = zif_uitb_c_alv_functions=>filter_menu ] ).
      DATA(lf_add_quickfilter) = abap_false.
      IF lv_index = 0.
        lv_index = line_index( e_object->mt_toolbar[ function = zif_uitb_c_alv_functions=>filter ] ).
        IF lv_index <> 0.
          lf_add_quickfilter = abap_true.
        ENDIF.
      ELSE.
        lf_add_quickfilter = abap_true.
      ENDIF.


      IF lf_add_quickfilter = abap_true.
        ADD 1 TO lv_index.
        INSERT VALUE #(
          butn_type = cntb_btype_dropdown
          function  = zif_uitb_c_alv_functions=>quickfilter_menu
          icon      = icon_select_with_condition
          quickinfo = 'Quickfilter...'
        ) INTO e_object->mt_toolbar INDEX lv_index.

        e_object->mt_btnmnu = VALUE #(
          BASE e_object->mt_btnmnu
          ( function = zif_uitb_c_alv_functions=>quickfilter_menu
            ctmenu   = mr_quickfilter_menu )
        ).

      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD on_user_command.
    set_function_call_active( ).

*.. Catch quick filter event
    IF e_ucomm = zif_uitb_c_alv_functions=>quickfilter_exclude OR
       e_ucomm = zif_uitb_c_alv_functions=>quickfilter_menu or
       e_ucomm = zif_uitb_c_alv_functions=>quickfilter.

      mr_controller->mr_model->perform_quick_filter(
          if_exclude = COND #( WHEN e_ucomm = zif_uitb_c_alv_functions=>quickfilter_exclude THEN abap_true )
      ).
    ELSE.
      zcl_uitb_alv_evt_controller=>raise_function_chosen(
          ir_controller = mr_controller
          iv_function   = e_ucomm
          iv_tag        = get_function_tag( iv_user_function = e_ucomm )
      ).
    ENDIF.

    set_function_call_active( abap_false ).
  ENDMETHOD.


  METHOD set_event_handlers.

    SET HANDLER:
            on_user_command FOR mr_grid,
            on_before_user_command FOR mr_grid,
            on_after_user_command FOR mr_grid,
            on_toolbar FOR mr_grid,
            on_menu_button FOR mr_grid,
            on_button_click FOR mr_grid,
            on_hotspot_click FOR mr_grid,
            on_double_click FOR mr_grid,
            on_context_menu FOR mr_grid,
            on_menu_button FOR mr_grid,
            on_f4 FOR mr_grid,
            on_data_changed FOR mr_grid,
            on_drag FOR mr_grid,
            on_drop FOR mr_grid,
            on_drop_get_flavor FOR mr_grid,
            on_drop_complete FOR mr_grid
            .

  ENDMETHOD.


  METHOD set_focus_to_grid.
    CHECK mr_grid IS BOUND.

    cl_gui_control=>set_focus( mr_grid ).
  ENDMETHOD.


  METHOD set_function.
    CHECK mr_grid IS BOUND.

    sy-ucomm = iv_function.

    mr_grid->fcode_bouncer( ).

    CLEAR sy-ucomm.

    mr_grid->set_user_command( space ).

  ENDMETHOD.


  METHOD set_function_call_active.
    mf_function_call_active = value.
  ENDMETHOD.


  METHOD set_metadata.
    DATA: ls_layout TYPE lvc_s_layo,
          lt_excluded type ui_functions,
          ls_variant type disvariant.

    FIELD-SYMBOLS: <lt_data> TYPE STANDARD TABLE.

    DATA(lr_model) = mr_controller->mr_model.

    ms_stable = is_stable.
    mf_keep_scroll_position = if_keep_scroll_position.

    IF mr_grid IS INITIAL.
      " initialize grid
      mr_grid = NEW cl_gui_alv_grid(
          i_parent        = lr_model->mr_container
          i_lifetime      = lr_model->mr_container->lifetime
          i_fcat_complete = abap_true
          i_appl_events   = abap_true
      ).

      " register event handlers
      set_event_handlers( ).
    ENDIF.

    " @TODO: determine refresh mode

    DATA(lr_columns) = lr_model->get_columns( ).
    DATA(lr_functional_settings) = lr_model->get_functional_settings( ).
    DATA(lr_dropdowns) = lr_functional_settings->get_dropdowns( ).
    DATA(lr_display_settings) = lr_model->get_display_settings( ).
    DATA(lr_functions) = lr_model->get_functions( ).
    DATA(lr_layout) = lr_model->get_layout( ).
    DATA(lr_filters) = lr_model->get_filters( ).
    DATA(lr_sorting) = lr_model->get_sorting( ).
    data(lr_data_changes) = lr_model->get_data_changes( ).
    DATA(lr_selections) = lr_model->get_selections( ).

    IF ir_changelist->has_metadata_changed( ) OR
       ir_changelist->is_new_data_requested( ).


      " get grid metadata for both new data and changed metadata
      zcl_uitb_alv_metadata_util=>set_alv_layout(
           EXPORTING ir_selections       = lr_selections
                     ir_display_settings = lr_display_settings
                     ir_columns          = lr_columns
           CHANGING  cs_layout           = ls_layout
      ).

      DATA(lt_fieldcat) = zcl_uitb_alv_metadata_util=>set_fieldcatalog(
        ir_display_settings = lr_display_settings
        ir_columns          = lr_columns
      ).

      DATA(lt_sort) = zcl_uitb_alv_metadata_util=>set_sorting( ir_sorts = lr_sorting ).

      DATA(lt_filters) = zcl_uitb_alv_metadata_util=>set_filters( ir_filters = lr_filters ).

      DATA(lt_f4) = zcl_uitb_alv_metadata_util=>set_f4_registrations( ir_columns = lr_columns ).
      mr_grid->register_f4_for_fields( lt_f4 ).

      IF lr_display_settings->is_editable( ).
        mr_grid->set_ready_for_input( 1 ).
      ELSE.
        mr_grid->set_ready_for_input( 0 ).
      ENDIF.
    ENDIF.


    CASE ir_changelist->is_new_data_requested( ).

      WHEN abap_false.

        IF ir_changelist->has_metadata_changed( ).
          mr_grid->set_frontend_fieldcatalog( lt_fieldcat ).
          mr_grid->set_filter_criteria( lt_filters ).
          mr_grid->set_frontend_layout( ls_layout ).
          mr_grid->set_sort_criteria( lt_sort ).
          mr_grid->set_drop_down_table( it_drop_down_alias = zcl_uitb_alv_metadata_util=>get_dropdowns( lr_dropdowns ) ).
        ENDIF.

        IF lr_display_settings->is_editable( ).
          mr_grid->register_edit_event( lr_data_changes->mv_edit_event ).
        ENDIF.

        IF ir_changelist->get_refresh_mode( ) <> zif_uitb_c_alv_refresh=>none.
          IF mf_keep_scroll_position = abap_true.
            DATA(lf_update_scroll_position) = abap_true.
            mr_grid->get_scroll_info_via_id(
              IMPORTING es_row_no   = DATA(ls_row_no)
                        es_col_info = DATA(ls_col_info)
            ).
          ENDIF.
        ENDIF.

        CASE ir_changelist->get_refresh_mode( ).

          WHEN zif_uitb_c_alv_refresh=>full.
            mr_grid->refresh_table_display(
                is_stable      = ms_stable
            ).

          WHEN zif_uitb_c_alv_refresh=>soft.
            mr_grid->refresh_table_display(
                is_stable      = ms_stable
                i_soft_refresh = abap_true
            ).
        ENDCASE.

      WHEN abap_true.
        mr_grid->set_drop_down_table( it_drop_down_alias = zcl_uitb_alv_metadata_util=>get_dropdowns( lr_dropdowns ) ).

        ASSIGN lr_model->mr_data->* TO <lt_data>.

        IF lr_display_settings->is_editable( ).
          mr_grid->register_edit_event( lr_data_changes->mv_edit_event ).
        ENDIF.

        DATA(lf_variant_default) = zcl_uitb_alv_metadata_util=>get_variant_default( lr_layout ).
        DATA(lv_variant_save) = zcl_uitb_alv_metadata_util=>get_variant_save( lr_layout ).
        zcl_uitb_alv_metadata_util=>get_variant(
          EXPORTING ir_layout  = lr_layout
          CHANGING  cs_variant = ls_variant
        ).

        mr_grid->set_table_for_first_display(
          EXPORTING
            is_layout                     = ls_layout
            is_variant                    = ls_variant
            i_save                        = lv_variant_save
            i_default                     = lf_variant_default
            it_toolbar_excluding          = lr_functions->get_disabled( )
          CHANGING
            it_outtab                     = <lt_data>
            it_fieldcatalog               = lt_fieldcat
            it_filter                     = lt_filters
            it_sort                       = lt_sort
          EXCEPTIONS
            invalid_parameter_combination = 1
            program_error                 = 2
            too_many_lines                = 3
            OTHERS                        = 4
        ).
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_uitb_alv_error
            EXPORTING
              textid = VALUE scx_t100key(
                  msgid = sy-msgid
                  msgno = sy-msgno
                  attr1 = 'MSGV1'
                  attr2 = 'MSGV2'
                  attr3 = 'MSGV3'
                  attr4 = 'MSGV4'
              )
              msgv1  = sy-msgv1
              msgv2  = sy-msgv2
              msgv3  = sy-msgv3
              msgv4  = sy-msgv4.
        ENDIF.

    ENDCASE.

    zcl_uitb_alv_selction_ctrller=>set_selections( mr_controller ).


    IF lf_update_scroll_position = abap_true.
      mr_grid->set_scroll_info_via_id(
          is_col_info = ls_col_info
          is_row_no   = ls_row_no
      ).
    ENDIF.

    cl_gui_cfw=>flush( ).
  ENDMETHOD.


  METHOD zif_uitb_alv_adpt_selections~get_current_cell.
    CHECK mr_grid IS BOUND.

    DATA(lr_model) = mr_controller->mr_model.

    zcl_uitb_alv_metadata_util=>get_current_cell(
        ir_grid       = mr_grid
        ir_selections = lr_model->mr_selections
    ).
  ENDMETHOD.


  METHOD zif_uitb_alv_adpt_selections~get_selected_cells.
    CHECK mr_grid IS BOUND.

    DATA(lr_model) = mr_controller->mr_model.

    zcl_uitb_alv_metadata_util=>get_selected_cells(
        ir_grid       = mr_grid
        ir_selections = lr_model->mr_selections
    ).
  ENDMETHOD.


  METHOD zif_uitb_alv_adpt_selections~get_selected_columns.
    CHECK mr_grid IS BOUND.

    DATA(lr_model) = mr_controller->mr_model.

    zcl_uitb_alv_metadata_util=>get_selected_columns(
        ir_grid       = mr_grid
        ir_selections = lr_model->mr_selections
    ).
  ENDMETHOD.


  METHOD zif_uitb_alv_adpt_selections~get_selected_rows.
    CHECK mr_grid IS BOUND.

    DATA(lr_model) = mr_controller->mr_model.

    zcl_uitb_alv_metadata_util=>get_selected_rows(
        ir_grid       = mr_grid
        ir_selections = lr_model->mr_selections
    ).
  ENDMETHOD.


  METHOD zif_uitb_alv_adpt_selections~set_current_cell.
    CHECK mr_grid IS BOUND.

    DATA(lr_model) = mr_controller->mr_model.

    zcl_uitb_alv_metadata_util=>set_current_cell(
        ir_grid       = mr_grid
        ir_selections = lr_model->mr_selections
    ).
  ENDMETHOD.


  METHOD zif_uitb_alv_adpt_selections~set_selected_cells.
    CHECK mr_grid IS BOUND.

    DATA(lr_model) = mr_controller->mr_model.

    zcl_uitb_alv_metadata_util=>set_selected_cells(
        ir_grid       = mr_grid
        ir_selections = lr_model->mr_selections
    ).
  ENDMETHOD.


  METHOD zif_uitb_alv_adpt_selections~set_selected_columns.
    CHECK mr_grid IS BOUND.

    DATA(lr_model) = mr_controller->mr_model.

    zcl_uitb_alv_metadata_util=>set_selected_cols(
        ir_grid       = mr_grid
        ir_selections = lr_model->mr_selections
    ).
  ENDMETHOD.


  METHOD zif_uitb_alv_adpt_selections~set_selected_rows.
    CHECK mr_grid IS BOUND.

    DATA(lr_model) = mr_controller->mr_model.

    zcl_uitb_alv_metadata_util=>set_selected_rows(
        ir_grid       = mr_grid
        ir_selections = lr_model->mr_selections
    ).
  ENDMETHOD.
ENDCLASS.
