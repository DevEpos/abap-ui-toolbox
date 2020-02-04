CLASS zcl_uitb_alv_grid_adapter DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  GLOBAL FRIENDS zif_uitb_alv_controller.

  PUBLIC SECTION.
    INTERFACES zif_uitb_alv_adpt_selections.
    METHODS constructor
      IMPORTING
        io_controller TYPE REF TO zcl_uitb_alv_controller.
    METHODS get_metadata.
    METHODS close.
    METHODS set_function
      IMPORTING
        iv_function TYPE ui_func.
    METHODS set_metadata
      IMPORTING
        is_stable               TYPE lvc_s_stbl OPTIONAL
        if_keep_scroll_position TYPE abap_bool OPTIONAL
        ir_changelist           TYPE REF TO zcl_uitb_alv_changelist OPTIONAL
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
    TYPES:
      BEGIN OF ty_s_context_function,
        function TYPE ui_func,
        text     TYPE string,
      END OF ty_s_context_function.
    TYPES: ty_t_context_function TYPE STANDARD TABLE OF ty_s_context_function WITH KEY function.
    TYPES:
      BEGIN OF ty_s_internal_ctx_func,
        functions     TYPE ty_t_context_function,
        insert_before TYPE ui_func,
        insert_after  TYPE ui_func,
      END OF ty_s_internal_ctx_func.
    TYPES: ty_t_internal_ctx_func TYPE STANDARD TABLE OF ty_s_internal_ctx_func WITH KEY insert_before insert_after.
    DATA mo_grid TYPE REF TO cl_gui_alv_grid.
    DATA mo_current_changelist TYPE REF TO zcl_uitb_alv_changelist.
    DATA mo_dialog TYPE REF TO if_alv_dialog.
    DATA ms_stable TYPE lvc_s_stbl.
    DATA mf_keep_scroll_position TYPE abap_bool.
    DATA mo_controller TYPE REF TO zcl_uitb_alv_controller.
    DATA mf_function_call_active TYPE abap_bool.
    DATA mo_quickfilter_menu TYPE REF TO cl_ctmenu.

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
    "! <p class="shorttext synchronized" lang="en">Deserialize menu from table</p>
    METHODS fill_menu_from_serialized
      IMPORTING
        it_menu_serialize       TYPE sctx_serialize
        it_additional_functions TYPE ty_t_internal_ctx_func
        io_menu                 TYPE REF TO cl_ctmenu.
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



CLASS zcl_uitb_alv_grid_adapter IMPLEMENTATION.


  METHOD constructor.
    mo_controller = io_controller.
    mo_quickfilter_menu = NEW cl_ctmenu( ).
    mo_quickfilter_menu->add_function(
        fcode = zif_uitb_c_alv_functions=>quickfilter_exclude
        text  = 'Exclude value'
    ).

*...... initialize dialog for alv display
    IF mo_controller->mo_model->mv_display_type = zif_uitb_c_alv_display_types=>dialog.
      mo_dialog = NEW cl_dialog( iv_title        = mo_controller->mo_model->get_display_settings( )->get_title( )
                                 io_grid_adapter = me ).
    ELSEIF mo_controller->mo_model->mv_display_type = zif_uitb_c_alv_display_types=>modal_dialog.
      mo_dialog = NEW cl_modal_dialog( iv_title = mo_controller->mo_model->get_display_settings( )->get_title( )
                                       io_grid_adapter = me ).
    ENDIF.
  ENDMETHOD.


  METHOD get_function_tag.
    DATA(lo_functions) = mo_controller->mo_model->get_functions( ).

    result = VALUE #( lo_functions->mt_function_tag_map[ function = iv_user_function ]-tag OPTIONAL ).
  ENDMETHOD.


  METHOD get_grid.
    result = mo_grid.
  ENDMETHOD.


  METHOD get_metadata.
    CHECK mo_grid IS BOUND.

    DATA(lo_model) = mo_controller->mo_model.

    DATA(lo_columns) = lo_model->get_columns( ).
    DATA(lo_functional_settings) = lo_model->get_functional_settings( ).
    DATA(lo_display_settings) = lo_model->get_display_settings( ).
    DATA(lo_selections) = lo_model->get_selections( ).
    DATA(lo_filters) = lo_model->get_filters( ).
    DATA(lo_sorting) = lo_model->get_sorting( ).
    DATA(lo_layout) = lo_model->get_layout( ).

    mo_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(lt_fcat) ).
    mo_grid->get_frontend_layout( IMPORTING es_layout = DATA(ls_layout) ).
    mo_grid->get_filter_criteria( IMPORTING et_filter = DATA(lt_filter) ).
    mo_grid->get_sort_criteria( IMPORTING et_sort = DATA(lt_sort) ).
    mo_grid->get_variant( IMPORTING es_variant = DATA(ls_variant) ).

    lo_selections->invalidate_selections( ).

    zcl_uitb_alv_metadata_util=>get_alv_layout(
        is_layout           = ls_layout
        io_selections       = lo_selections
        io_display_settings = lo_display_settings
        io_columns          = lo_columns
    ).

    zcl_uitb_alv_metadata_util=>get_fieldcatalog(
        it_fieldcat = lt_fcat
        io_columns  = lo_columns
    ).

    zcl_uitb_alv_metadata_util=>get_filters(
        io_filters = lo_filters
        it_filter  = lt_filter
    ).

    zcl_uitb_alv_metadata_util=>get_sorting(
        io_sorts = lo_sorting
        it_sorts = lt_sort
    ).

    zcl_uitb_alv_metadata_util=>set_variant(
        is_variant = ls_variant
        io_layout  = lo_layout
    ).

  ENDMETHOD.


  METHOD is_function_call_active.
    result = mf_function_call_active.
  ENDMETHOD.


  METHOD on_after_user_command.
    set_function_call_active( ).

    zcl_uitb_alv_evt_controller=>raise_after_function(
        io_controller = mo_controller
        iv_function   = e_ucomm
    ).

    set_function_call_active( abap_false ).
  ENDMETHOD.


  METHOD on_before_user_command.
    set_function_call_active( ).

    zcl_uitb_alv_evt_controller=>raise_before_function(
        io_controller = mo_controller
        iv_function   = e_ucomm
    ).

    set_function_call_active( abap_false ).
  ENDMETHOD.


  METHOD on_button_click.
    set_function_call_active( ).

    zcl_uitb_alv_evt_controller=>raise_link_click(
        io_controller = mo_controller
        iv_column     = es_col_id-fieldname
        iv_row        = es_row_no-row_id
    ).

    set_function_call_active( abap_false ).
  ENDMETHOD.


  METHOD on_context_menu.
    DATA: lt_serialized_menu TYPE sctx_serialize.
    DATA(lo_model) = mo_controller->mo_model.

    DATA(lo_functions) = lo_model->get_functions( ).
    IF lo_functions IS BOUND.
      DATA(lt_disabled) = lo_functions->get_disabled( ).
      e_object->hide_functions( lt_disabled ).

      IF lo_functions->mf_quickfilter = abap_true.
*...... Recreate menu and add quick filter actions
        e_object->get_functions( IMPORTING fcodes = DATA(lt_fcodes) ).
        e_object->if_ctxmnu_internal~serialize_menu( CHANGING menu = lt_serialized_menu ).
        e_object->clear( ).
        fill_menu_from_serialized(
            it_menu_serialize       = lt_serialized_menu
            it_additional_functions = VALUE #(
             (  insert_after = zif_uitb_c_alv_functions=>filter
                functions    = VALUE #(
                  ( function     = zif_uitb_c_alv_functions=>quickfilter
                    text         = |{ 'Quickfilter' }| )
                  ( function     = zif_uitb_c_alv_functions=>quickfilter_exclude
                    text         = |{ 'Quickfilter (Exclude value)' }| )
                )
              )
            )
            io_menu                 = e_object
        ).
      ENDIF.
    ENDIF.

    set_function_call_active( ).

    zcl_uitb_alv_evt_controller=>raise_context_menu(
        io_controller = mo_controller
        io_menu       = e_object
    ).

    set_function_call_active( abap_false ).

  ENDMETHOD.


  METHOD on_data_changed.
    set_function_call_active( ).

    zcl_uitb_alv_evt_controller=>raise_data_changed(
        io_controller      = mo_controller
        io_change_protocol = er_data_changed
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
        io_controller = mo_controller
        iv_column     = e_column-fieldname
        iv_row        = es_row_no-row_id
    ).

    set_function_call_active( abap_false ).
  ENDMETHOD.


  METHOD on_drag.
    zcl_uitb_alv_evt_controller=>raise_drag(
        io_controller  = mo_controller
        iv_row         = es_row_no-row_id
        iv_column      = e_column-fieldname
        is_row_no      = es_row_no
        io_dragdropobj = e_dragdropobj
    ).
  ENDMETHOD.


  METHOD on_drop.
    zcl_uitb_alv_evt_controller=>raise_drop(
        io_controller  = mo_controller
        iv_row         = es_row_no-row_id
        iv_column      = e_column-fieldname
        is_row_no      = es_row_no
        io_dragdropobj = e_dragdropobj
    ).
  ENDMETHOD.


  METHOD on_drop_complete.
    zcl_uitb_alv_evt_controller=>raise_drop_complete(
        io_controller  = mo_controller
        iv_row         = es_row_no-row_id
        iv_column      = e_column-fieldname
        is_row_no      = es_row_no
        io_dragdropobj = e_dragdropobj
    ).
  ENDMETHOD.


  METHOD on_drop_get_flavor.
    zcl_uitb_alv_evt_controller=>raise_drop_get_flavor(
        io_controller  = mo_controller
        iv_row         = es_row_no-row_id
        iv_column      = e_column-fieldname
        is_row_no      = es_row_no
        io_dragdropobj = e_dragdropobj
        it_flavors     = e_flavors
    ).
  ENDMETHOD.


  METHOD on_f4.
    set_function_call_active( ).

    zcl_uitb_alv_evt_controller=>raise_f4(
        io_controller = mo_controller
        iv_fieldname  = e_fieldname
        iv_fieldvalue = e_fieldvalue
        is_row_no     = es_row_no
        io_event_data = er_event_data
        it_bad_cells  = et_bad_cells
        if_display    = e_display
    ).

    set_function_call_active( abap_false ).
  ENDMETHOD.


  METHOD on_hotspot_click.
    set_function_call_active( ).

    zcl_uitb_alv_evt_controller=>raise_link_click(
        io_controller = mo_controller
        iv_column     = e_column_id-fieldname
        iv_row        = es_row_no-row_id
    ).

    set_function_call_active( abap_false ).
  ENDMETHOD.


  METHOD on_menu_button.
  ENDMETHOD.


  METHOD on_toolbar.
    DATA: lt_disabled_selopt TYPE RANGE OF ui_func.

    DATA(lo_model) = mo_controller->mo_model.

    DATA(lo_functions) = lo_model->get_functions( ).
    IF lo_functions IS NOT BOUND.
      RETURN.
    ENDIF.

    DATA(lt_disabled) = lo_functions->get_disabled( ).

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
    IF lo_functions->mt_buttons_front IS NOT INITIAL.
      LOOP AT lo_functions->mt_buttons_front ASSIGNING FIELD-SYMBOL(<ls_button_front>).
        INSERT CORRESPONDING #( <ls_button_front> ) INTO e_object->mt_toolbar INDEX 1.
      ENDLOOP.
    ENDIF.

*.. append toolbar buttons at the end
    e_object->mt_toolbar = VALUE #( BASE e_object->mt_toolbar
      ( LINES OF CORRESPONDING #( lo_functions->mt_buttons ) )
    ).

    IF lo_functions->mt_menus IS NOT INITIAL.
      e_object->mt_btnmnu = VALUE #(
        BASE e_object->mt_btnmnu
        ( LINES OF CORRESPONDING #( lo_functions->mt_menus ) )
      ).
    ENDIF.

*.. add quickfilter if set inside functions
    IF lo_functions->mf_quickfilter = abap_true.

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
            ctmenu   = mo_quickfilter_menu )
        ).

      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD on_user_command.
    set_function_call_active( ).

*.. Catch quick filter event
    IF e_ucomm = zif_uitb_c_alv_functions=>quickfilter_exclude OR
       e_ucomm = zif_uitb_c_alv_functions=>quickfilter_menu OR
       e_ucomm = zif_uitb_c_alv_functions=>quickfilter.

      zcl_uitb_alv_evt_controller=>handle_before_event( io_controller = mo_controller ).
      mo_controller->mo_model->perform_quick_filter(
          if_exclude = COND #( WHEN e_ucomm = zif_uitb_c_alv_functions=>quickfilter_exclude THEN abap_true )
      ).
      zcl_uitb_alv_evt_controller=>handle_after_event( io_controller = mo_controller ).
    ELSE.
      zcl_uitb_alv_evt_controller=>raise_function_chosen(
          io_controller = mo_controller
          iv_function   = e_ucomm
          iv_tag        = get_function_tag( iv_user_function = e_ucomm )
      ).
    ENDIF.

    set_function_call_active( abap_false ).
  ENDMETHOD.

  METHOD fill_menu_from_serialized.
    FIELD-SYMBOLS: <ls_serialized> LIKE LINE OF it_menu_serialize.

    DEFINE add_serialized_function.
      io_menu->add_function(
          fcode       = <ls_serialized>-fcode
          text        = <ls_serialized>-text
          disabled    = <ls_serialized>-disabled
          checked     = <ls_serialized>-checked
          accelerator = <ls_serialized>-accelerator
      ).
    END-OF-DEFINITION.

    LOOP AT it_menu_serialize ASSIGNING <ls_serialized>.
      IF <ls_serialized>-type = sctx_c_type_separator.
        io_menu->add_separator( ).
      ELSEIF <ls_serialized>-type = sctx_c_type_function.
*..... Check if an additional function should be inserted before or after
        LOOP AT it_additional_functions ASSIGNING FIELD-SYMBOL(<ls_add_func>) WHERE insert_after = <ls_serialized>-fcode
                                                                                 OR insert_before = <ls_serialized>-fcode.
        ENDLOOP.
        IF sy-subrc = 0.
          IF <ls_add_func>-insert_after IS NOT INITIAL.
            add_serialized_function.
            io_menu->add_separator( ).
          ENDIF.
          LOOP AT <ls_add_func>-functions ASSIGNING FIELD-SYMBOL(<ls_ctx_func>).
            io_menu->add_function(
                fcode = <ls_ctx_func>-function
                text  = |{ <ls_ctx_func>-text }|
            ).
          ENDLOOP.
          IF <ls_add_func>-insert_before = abap_true.
            io_menu->add_separator( ).
            add_serialized_function.
          ENDIF.
        ELSE.
          add_serialized_function.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD set_event_handlers.

    SET HANDLER:
            on_user_command FOR mo_grid,
            on_before_user_command FOR mo_grid,
            on_after_user_command FOR mo_grid,
            on_toolbar FOR mo_grid,
            on_menu_button FOR mo_grid,
            on_button_click FOR mo_grid,
            on_hotspot_click FOR mo_grid,
            on_double_click FOR mo_grid,
            on_context_menu FOR mo_grid,
            on_menu_button FOR mo_grid,
            on_f4 FOR mo_grid,
            on_data_changed FOR mo_grid,
            on_drag FOR mo_grid,
            on_drop FOR mo_grid,
            on_drop_get_flavor FOR mo_grid,
            on_drop_complete FOR mo_grid
            .

  ENDMETHOD.



  METHOD set_focus_to_grid.
    CHECK mo_grid IS BOUND.

    cl_gui_control=>set_focus( mo_grid ).
  ENDMETHOD.

  METHOD close.
    CHECK mo_dialog IS BOUND.
    mo_dialog->zif_uitb_gui_screen~leave_screen( ).
  ENDMETHOD.


  METHOD set_function.
    CHECK mo_grid IS BOUND.

    sy-ucomm = iv_function.

    mo_grid->fcode_bouncer( ).

    CLEAR sy-ucomm.

    mo_grid->set_user_command( space ).

  ENDMETHOD.


  METHOD set_function_call_active.
    mf_function_call_active = value.
  ENDMETHOD.


  METHOD set_metadata.
    DATA: ls_layout   TYPE lvc_s_layo,
          lt_excluded TYPE ui_functions,
          ls_variant  TYPE disvariant.

    FIELD-SYMBOLS: <lt_data> TYPE STANDARD TABLE.

    DATA(lo_model) = mo_controller->mo_model.
    DATA(lo_data_changes) = lo_model->get_data_changes( ).

    ms_stable = is_stable.
    mf_keep_scroll_position = if_keep_scroll_position.
    IF ir_changelist IS SUPPLIED.
      mo_current_changelist = ir_changelist.
    ENDIF.

    IF mo_grid IS INITIAL.
      IF mo_dialog IS BOUND.

        IF mo_dialog->mf_visible = abap_false.
          mo_dialog->show(
              iv_top    = lo_model->ms_popup_dimensions-top
              iv_left   = lo_model->ms_popup_dimensions-left
              iv_width  = lo_model->ms_popup_dimensions-width
              iv_height = lo_model->ms_popup_dimensions-height
          ).
          IF mo_dialog->mf_modal = abap_true.
            RETURN.
          ENDIF.
        ENDIF.

        mo_grid = mo_dialog->mo_gui_alv_grid.
      ELSE.
        " initialize grid
        mo_grid = NEW cl_gui_alv_grid(
            i_parent        = lo_model->mr_container
            i_lifetime      = lo_model->mr_container->lifetime
            i_fcat_complete = abap_true
            i_appl_events   = abap_true
            i_applogparent  = lo_data_changes->mo_applog_container
        ).

      ENDIF.

      " register event handlers
      set_event_handlers( ).
    ENDIF.

    DATA(lo_columns) = lo_model->get_columns( ).
    DATA(lo_functional_settings) = lo_model->get_functional_settings( ).
    DATA(lo_dropdowns) = lo_functional_settings->get_dropdowns( ).
    DATA(lo_display_settings) = lo_model->get_display_settings( ).
    DATA(lo_functions) = lo_model->get_functions( ).
    DATA(lo_layout) = lo_model->get_layout( ).
    DATA(lo_filters) = lo_model->get_filters( ).
    DATA(lo_sorting) = lo_model->get_sorting( ).
    DATA(lo_selections) = lo_model->get_selections( ).

    IF mo_current_changelist->has_metadata_changed( ) OR
       mo_current_changelist->is_new_data_requested( ).


      " get grid metadata for both new data and changed metadata
      zcl_uitb_alv_metadata_util=>set_alv_layout(
           EXPORTING ir_selections       = lo_selections
                     ir_display_settings = lo_display_settings
                     ir_columns          = lo_columns
           CHANGING  cs_layout           = ls_layout
      ).

      IF mo_controller->mo_model->mv_display_type <> zif_uitb_c_alv_display_types=>embedded.
        CLEAR ls_layout-grid_title.
      ENDIF.

      DATA(lt_fieldcat) = zcl_uitb_alv_metadata_util=>set_fieldcatalog(
        ir_display_settings = lo_display_settings
        ir_columns          = lo_columns
      ).

      DATA(lt_sort) = zcl_uitb_alv_metadata_util=>set_sorting( ir_sorts = lo_sorting ).

      DATA(lt_filters) = zcl_uitb_alv_metadata_util=>set_filters( ir_filters = lo_filters ).

      DATA(lt_f4) = zcl_uitb_alv_metadata_util=>set_f4_registrations( ir_columns = lo_columns ).
      mo_grid->register_f4_for_fields( lt_f4 ).

      IF lo_display_settings->is_editable( ).
        mo_grid->set_ready_for_input( 1 ).
      ELSE.
        mo_grid->set_ready_for_input( 0 ).
      ENDIF.

      mo_grid->process_ucomm_on_invalid_input( lo_functions->mt_func_on_invalid_input ).
    ENDIF.


    CASE mo_current_changelist->is_new_data_requested( ).

      WHEN abap_false.

        IF mo_current_changelist->has_metadata_changed( ).
          IF mo_current_changelist->is_filters_only_change( ).
            mo_grid->set_filter_criteria( lt_filters ).
          ELSEIF mo_current_changelist->is_layout_only_change( ).
            mo_grid->set_frontend_layout( ls_layout ).
          ELSE.
            mo_grid->set_frontend_fieldcatalog( lt_fieldcat ).
            mo_grid->set_filter_criteria( lt_filters ).
            mo_grid->set_frontend_layout( ls_layout ).
            mo_grid->set_sort_criteria( lt_sort ).
            mo_grid->set_drop_down_table( it_drop_down_alias = zcl_uitb_alv_metadata_util=>get_dropdowns( lo_dropdowns ) ).
          ENDIF.
        ENDIF.

        IF lo_display_settings->is_editable( ).
          mo_grid->register_edit_event( lo_data_changes->mv_edit_event ).
        ENDIF.

        IF mo_current_changelist->get_refresh_mode( ) <> zif_uitb_c_alv_refresh=>none.
          IF mf_keep_scroll_position = abap_true.
            DATA(lf_update_scroll_position) = abap_true.
            mo_grid->get_scroll_info_via_id(
              IMPORTING es_row_no   = DATA(ls_row_no)
                        es_col_info = DATA(ls_col_info)
            ).
          ENDIF.
        ENDIF.

        CASE mo_current_changelist->get_refresh_mode( ).

          WHEN zif_uitb_c_alv_refresh=>full.
            mo_grid->refresh_table_display(
                is_stable      = ms_stable
            ).

          WHEN zif_uitb_c_alv_refresh=>soft.
            mo_grid->refresh_table_display(
                is_stable      = ms_stable
                i_soft_refresh = abap_true
            ).
        ENDCASE.

      WHEN abap_true.
        mo_grid->set_drop_down_table( it_drop_down_alias = zcl_uitb_alv_metadata_util=>get_dropdowns( lo_dropdowns ) ).

        ASSIGN lo_model->mr_data->* TO <lt_data>.

        IF lo_display_settings->is_editable( ).
          mo_grid->register_edit_event( lo_data_changes->mv_edit_event ).
        ENDIF.

        DATA(lf_variant_default) = zcl_uitb_alv_metadata_util=>get_variant_default( lo_layout ).
        DATA(lv_variant_save) = zcl_uitb_alv_metadata_util=>get_variant_save( lo_layout ).
        zcl_uitb_alv_metadata_util=>get_variant(
          EXPORTING ir_layout  = lo_layout
          CHANGING  cs_variant = ls_variant
        ).

        mo_grid->set_table_for_first_display(
          EXPORTING
            is_layout                     = ls_layout
            is_variant                    = ls_variant
            i_save                        = lv_variant_save
            i_default                     = lf_variant_default
            it_toolbar_excluding          = lo_functions->get_disabled( )
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

    zcl_uitb_alv_selction_ctrller=>set_selections( mo_controller ).


    IF lf_update_scroll_position = abap_true.
      mo_grid->set_scroll_info_via_id(
          is_col_info = ls_col_info
          is_row_no   = ls_row_no
      ).
    ENDIF.

    cl_gui_cfw=>flush( ).
  ENDMETHOD.


  METHOD zif_uitb_alv_adpt_selections~get_current_cell.
    CHECK mo_grid IS BOUND.

    DATA(lo_model) = mo_controller->mo_model.

    zcl_uitb_alv_metadata_util=>get_current_cell(
        ir_grid       = mo_grid
        ir_selections = lo_model->mo_selections
    ).
  ENDMETHOD.


  METHOD zif_uitb_alv_adpt_selections~get_selected_cells.
    CHECK mo_grid IS BOUND.

    DATA(lo_model) = mo_controller->mo_model.

    zcl_uitb_alv_metadata_util=>get_selected_cells(
        ir_grid       = mo_grid
        ir_selections = lo_model->mo_selections
    ).
  ENDMETHOD.


  METHOD zif_uitb_alv_adpt_selections~get_selected_columns.
    CHECK mo_grid IS BOUND.

    DATA(lo_model) = mo_controller->mo_model.

    zcl_uitb_alv_metadata_util=>get_selected_columns(
        ir_grid       = mo_grid
        ir_selections = lo_model->mo_selections
    ).
  ENDMETHOD.


  METHOD zif_uitb_alv_adpt_selections~get_selected_rows.
    CHECK mo_grid IS BOUND.

    DATA(lo_model) = mo_controller->mo_model.

    zcl_uitb_alv_metadata_util=>get_selected_rows(
        ir_grid       = mo_grid
        ir_selections = lo_model->mo_selections
    ).
  ENDMETHOD.


  METHOD zif_uitb_alv_adpt_selections~set_current_cell.
    CHECK mo_grid IS BOUND.

    DATA(lo_model) = mo_controller->mo_model.

    zcl_uitb_alv_metadata_util=>set_current_cell(
        ir_grid       = mo_grid
        ir_selections = lo_model->mo_selections
    ).
  ENDMETHOD.


  METHOD zif_uitb_alv_adpt_selections~set_selected_cells.
    CHECK mo_grid IS BOUND.

    DATA(lo_model) = mo_controller->mo_model.

    zcl_uitb_alv_metadata_util=>set_selected_cells(
        ir_grid       = mo_grid
        ir_selections = lo_model->mo_selections
    ).
  ENDMETHOD.


  METHOD zif_uitb_alv_adpt_selections~set_selected_columns.
    CHECK mo_grid IS BOUND.

    DATA(lo_model) = mo_controller->mo_model.

    zcl_uitb_alv_metadata_util=>set_selected_cols(
        ir_grid       = mo_grid
        ir_selections = lo_model->mo_selections
    ).
  ENDMETHOD.


  METHOD zif_uitb_alv_adpt_selections~set_selected_rows.
    CHECK mo_grid IS BOUND.

    DATA(lo_model) = mo_controller->mo_model.

    zcl_uitb_alv_metadata_util=>set_selected_rows(
        ir_grid       = mo_grid
        ir_selections = lo_model->mo_selections
    ).
  ENDMETHOD.
ENDCLASS.
