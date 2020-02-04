CLASS zcl_uitb_alv_metadata_util DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS get_variant
      IMPORTING
        !ir_layout  TYPE REF TO zcl_uitb_alv_layout
      CHANGING
        !cs_variant TYPE disvariant .
    CLASS-METHODS set_variant
      IMPORTING
        !is_variant TYPE disvariant
        !io_layout  TYPE REF TO zcl_uitb_alv_layout .
    CLASS-METHODS get_variant_default
      IMPORTING
        !ir_layout        TYPE REF TO zcl_uitb_alv_layout
      RETURNING
        VALUE(rf_default) TYPE abap_bool .
    CLASS-METHODS get_variant_save
      IMPORTING
        !ir_layout     TYPE REF TO zcl_uitb_alv_layout
      RETURNING
        VALUE(rv_save) TYPE char1 .
    CLASS-METHODS get_dropdowns
      IMPORTING
        !ir_dropdowns            TYPE REF TO zcl_uitb_alv_dropdowns
      RETURNING
        VALUE(rt_dropdown_alias) TYPE lvc_t_dral .
    CLASS-METHODS set_alv_layout
      IMPORTING
        !ir_selections       TYPE REF TO zcl_uitb_alv_selections
        !ir_display_settings TYPE REF TO zcl_uitb_alv_display_settings
        !ir_columns          TYPE REF TO zcl_uitb_alv_columns
      CHANGING
        !cs_layout           TYPE lvc_s_layo .
    CLASS-METHODS set_fieldcatalog
      IMPORTING
        !ir_columns          TYPE REF TO zcl_uitb_alv_columns
        !ir_display_settings TYPE REF TO zcl_uitb_alv_display_settings
      RETURNING
        VALUE(rt_fieldcat)   TYPE lvc_t_fcat .
    CLASS-METHODS set_filters
      IMPORTING
        !ir_filters       TYPE REF TO zcl_uitb_alv_filters
      RETURNING
        VALUE(rt_filters) TYPE lvc_t_filt .
    CLASS-METHODS set_sorting
      IMPORTING
        !ir_sorts     TYPE REF TO zcl_uitb_alv_sorts
      RETURNING
        VALUE(result) TYPE lvc_t_sort .
    CLASS-METHODS get_sorting
      IMPORTING
        !io_sorts TYPE REF TO zcl_uitb_alv_sorts
        !it_sorts TYPE lvc_t_sort .
    CLASS-METHODS get_filters
      IMPORTING
        !io_filters TYPE REF TO zcl_uitb_alv_filters
        !it_filter  TYPE lvc_t_filt .
    CLASS-METHODS set_f4_registrations
      IMPORTING
        !ir_columns  TYPE REF TO zcl_uitb_alv_columns
      RETURNING
        VALUE(rt_f4) TYPE lvc_t_f4 .
    CLASS-METHODS get_fieldcatalog
      IMPORTING
        !it_fieldcat TYPE lvc_t_fcat
        !io_columns  TYPE REF TO zcl_uitb_alv_columns .
    CLASS-METHODS get_alv_layout
      IMPORTING
        !is_layout           TYPE lvc_s_layo
        !io_selections       TYPE REF TO zcl_uitb_alv_selections
        !io_display_settings TYPE REF TO zcl_uitb_alv_display_settings
        !io_columns          TYPE REF TO zcl_uitb_alv_columns .
    CLASS-METHODS get_selected_cells
      IMPORTING
        !ir_grid          TYPE REF TO cl_gui_alv_grid
        !ir_selections    TYPE REF TO zcl_uitb_alv_selections
      RETURNING
        VALUE(rf_changed) TYPE abap_bool .
    CLASS-METHODS get_selected_columns
      IMPORTING
        !ir_grid          TYPE REF TO cl_gui_alv_grid
        !ir_selections    TYPE REF TO zcl_uitb_alv_selections
      RETURNING
        VALUE(rf_changed) TYPE abap_bool .
    CLASS-METHODS get_selected_rows
      IMPORTING
        !ir_grid          TYPE REF TO cl_gui_alv_grid
        !ir_selections    TYPE REF TO zcl_uitb_alv_selections
      RETURNING
        VALUE(rf_changed) TYPE abap_bool .
    CLASS-METHODS set_current_cell
      IMPORTING
        !ir_grid       TYPE REF TO cl_gui_alv_grid
        !ir_selections TYPE REF TO zcl_uitb_alv_selections .
    CLASS-METHODS set_selected_cells
      IMPORTING
        !ir_grid       TYPE REF TO cl_gui_alv_grid
        !ir_selections TYPE REF TO zcl_uitb_alv_selections .
    CLASS-METHODS set_selected_cols
      IMPORTING
        !ir_grid       TYPE REF TO cl_gui_alv_grid
        !ir_selections TYPE REF TO zcl_uitb_alv_selections .
    CLASS-METHODS set_selected_rows
      IMPORTING
        !ir_grid       TYPE REF TO cl_gui_alv_grid
        !ir_selections TYPE REF TO zcl_uitb_alv_selections .
    CLASS-METHODS get_current_cell
      IMPORTING
        !ir_grid          TYPE REF TO cl_gui_alv_grid
        !ir_selections    TYPE REF TO zcl_uitb_alv_selections
      RETURNING
        VALUE(rf_changed) TYPE abap_bool .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_uitb_alv_metadata_util IMPLEMENTATION.


  METHOD get_alv_layout.
    IF io_columns IS BOUND.
      io_columns->mv_color_column_simple = is_layout-info_fname.
      io_columns->mv_style_column = is_layout-stylefname.
      io_columns->mf_optimized = is_layout-cwidth_opt.
      io_columns->mf_key_fixation = xsdbool( is_layout-no_keyfix = abap_false ).
      io_columns->mf_single_click_sort = is_layout-sgl_clk_hd.
      io_columns->mf_col_header_visible = xsdbool( is_layout-no_headers = abap_false ).
    ENDIF.

    IF io_display_settings IS BOUND.
      io_display_settings->mf_no_merging = is_layout-no_merging.
      io_display_settings->mf_hide_toolbar = is_layout-no_toolbar.
      io_display_settings->mf_no_row_insertions = is_layout-no_rowins.
      io_display_settings->mf_striped = is_layout-zebra.
      io_display_settings->mf_no_row_marks = is_layout-no_rowmark.
      io_display_settings->mf_no_row_moves = is_layout-no_rowmove.
      io_display_settings->mv_title = is_layout-grid_title.
      io_display_settings->mf_smalltitle = is_layout-smalltitle.
      io_display_settings->ms_dnd = is_layout-s_dragdrop.
    ENDIF.

    IF io_selections IS BOUND.
      CASE is_layout-sel_mode.
        WHEN 'B'.
          io_selections->mv_mode = zif_uitb_c_alv_selection=>single.

        WHEN 'C'.
          io_selections->mv_mode = zif_uitb_c_alv_selection=>multiple.

        WHEN 'D'.
          io_selections->mv_mode = zif_uitb_c_alv_selection=>cell.

        WHEN 'A'.
          io_selections->mv_mode = zif_uitb_c_alv_selection=>row_column.
      ENDCASE.
    ENDIF.
  ENDMETHOD.


  METHOD get_current_cell.
    DATA: lv_value TYPE lvc_value.

***    CHECK cl_salv_model=>is_offline( ) EQ false.

    CHECK ir_grid IS BOUND.

    ir_grid->get_current_cell(
      IMPORTING es_col_id = DATA(ls_col_id)
                es_row_no = DATA(ls_row_no)
                e_value   = lv_value
    ).

    DATA(ls_cell) = VALUE zif_uitb_alv_types=>ty_cell(
      row    = ls_row_no-row_id
      column = ls_col_id-fieldname
    ).

    IF ir_selections->ms_current_cell NE ls_cell.
      rf_changed = abap_true.
    ENDIF.

    ir_selections->ms_current_cell = ls_cell.

    ir_selections->mf_ccell_received = abap_true.

    IF ls_cell IS NOT INITIAL.

      ir_selections->mr_controller->set_changed(
        iv_name     = ir_selections->mv_name
        iv_method   = 'SET_CURRENT_CELL'
        iv_object   = zif_uitb_c_alv_selection_type=>rows
        ir_ref      = ir_selections
        iv_flavour  = zif_uitb_c_alv_chglist_flavor=>selections
        iv_frontend = abap_true
      ).

    ENDIF.
  ENDMETHOD.


  METHOD get_dropdowns.
    CHECK ir_dropdowns IS BOUND.

    rt_dropdown_alias = VALUE #(
      FOR <ls_dropdown> IN ir_dropdowns->get( )
      ( LINES OF <ls_dropdown>-dropdown_ref->get_values( ) )
    ).
  ENDMETHOD.


  METHOD get_fieldcatalog.
    DATA(lt_fieldcat) = it_fieldcat.
    SORT lt_fieldcat STABLE BY col_pos ASCENDING.

    LOOP AT lt_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).
      DATA(lv_tabix) = sy-tabix.

      io_columns->update_column_data(
        iv_columnname = <ls_fcat>-fieldname
        is_data       = <ls_fcat>
      ).

      io_columns->set_column_position( iv_columnname = <ls_fcat>-fieldname iv_position = lv_tabix ).
    ENDLOOP.
  ENDMETHOD.


  METHOD get_filters.
    CHECK io_filters IS BOUND.

    io_filters->clear( ).

    LOOP AT it_filter ASSIGNING FIELD-SYMBOL(<ls_filter>).
      TRY.
          DATA(lr_filter) = io_filters->get_filter( <ls_filter>-fieldname ).
        CATCH zcx_uitb_alv_not_found.
          TRY.
              lr_filter = io_filters->add_filter(
                iv_columnname = <ls_filter>-fieldname
              ).
            CATCH zcx_uitb_alv_not_found zcx_uitb_alv_existing.
              CONTINUE.
          ENDTRY.
      ENDTRY.

      CHECK lr_filter IS BOUND.

      lr_filter->add_selopt(
          iv_sign   = <ls_filter>-sign
          iv_option = <ls_filter>-option
          iv_low    = CONV #( <ls_filter>-low )
          iv_high   = CONV #( <ls_filter>-high )
      ).
    ENDLOOP.
  ENDMETHOD.


  METHOD get_selected_cells.

***    CHECK cl_salv_model=>is_offline( ) EQ false.

    CHECK ir_grid IS BOUND.

    ir_grid->get_selected_cells( IMPORTING et_cell = DATA(lt_cells) ).

    cl_gui_cfw=>flush( ).

    DATA(lt_alv_model_cell) = VALUE zif_uitb_alv_types=>tt_cell(
      FOR <ls_cell> IN lt_cells
      ( row    = <ls_cell>-row_id-index
        column = <ls_cell>-col_id-fieldname
***        value = <ls_cell>-value
        )
    ).

    IF ir_selections->mt_selected_cells NE lt_alv_model_cell.
      rf_changed = abap_true.
    ENDIF.

    ir_selections->mt_selected_cells = lt_alv_model_cell.
    ir_selections->mf_ccell_received = abap_true.

    IF lt_alv_model_cell IS NOT INITIAL.
      ir_selections->mr_controller->set_changed(
        iv_name     = ir_selections->mv_name
        iv_method   = 'SET_SELECTED_CELLS'
        iv_object   = zif_uitb_c_alv_selection_type=>cells
        ir_ref      = ir_selections
        iv_frontend = abap_true
        iv_flavour  = zif_uitb_c_alv_chglist_flavor=>selections ).

    ENDIF.
  ENDMETHOD.


  METHOD get_selected_columns.

***    CHECK cl_salv_model=>is_offline( ) EQ false.

    CHECK ir_grid IS BOUND.

    ir_grid->get_selected_columns( IMPORTING et_index_columns = DATA(lt_cols) ).

    DATA(lt_alv_cols) = VALUE lvc_t_fnam(
      FOR <ls_col> IN lt_cols
      ( <ls_col>-fieldname )
    ).

    IF ir_selections->mt_selected_columns NE lt_alv_cols.
      rf_changed = abap_true.
    ENDIF.

    ir_selections->mt_selected_columns = lt_alv_cols.
    ir_selections->mf_cols_received = abap_true.

    IF lt_alv_cols IS NOT INITIAL.

      ir_selections->mr_controller->set_changed(
        iv_name     = ir_selections->mv_name
        iv_method   = 'SET_SELECTED_COLUMNS'
        iv_object   = zif_uitb_c_alv_selection_type=>columns
        ir_ref      = ir_selections
        iv_frontend = abap_true
        iv_flavour  = zif_uitb_c_alv_chglist_flavor=>selections ).

    ENDIF.
  ENDMETHOD.


  METHOD get_selected_rows.

***    CHECK cl_salv_model=>is_offline( ) EQ false.

    CHECK ir_grid IS BOUND.

    ir_grid->get_selected_rows( IMPORTING et_row_no = DATA(lt_rows) ).

    DATA(lt_alv_rows) = VALUE lvc_t_rows(
      FOR <ls_row> IN lt_rows
      " mt_selected_rows has not the subtype property and does not allow
      " negative values, therefore we have to delete subtotals and Totals
      WHERE ( row_id > 0 )
      ( <ls_row>-row_id )
    ).

    IF ir_selections->mt_selected_rows NE lt_alv_rows.
      rf_changed = abap_true.
    ENDIF.

    ir_selections->mt_selected_rows = lt_alv_rows.
    ir_selections->mf_rows_received = abap_true.

    IF lt_alv_rows IS NOT INITIAL.

      ir_selections->mr_controller->set_changed(
        iv_name    = ir_selections->mv_name
        iv_method  = 'SET_SELECTED_ROWS'
        iv_object  = zif_uitb_c_alv_selection_type=>rows
        ir_ref     = ir_selections
        iv_frontend = abap_true
        iv_flavour = zif_uitb_c_alv_chglist_flavor=>selections ).

    ENDIF.
  ENDMETHOD.


  METHOD get_sorting.
    DATA(lt_sort) = it_sorts.

    SORT lt_sort BY spos ASCENDING.
    LOOP AT lt_sort ASSIGNING FIELD-SYMBOL(<ls_sort>).
      TRY.
          DATA(lr_sort_existing) = io_sorts->get_sort( iv_column_name = <ls_sort>-fieldname ).
          lr_sort_existing->mv_sequence =
            COND #( WHEN <ls_sort>-down = abap_true THEN
                      zif_uitb_c_alv_sorting=>descending
                    ELSE
                      zif_uitb_c_alv_sorting=>ascending  ).
          lr_sort_existing->mf_subtotal = <ls_sort>-subtot.
        CATCH zcx_uitb_alv_not_found.
          " add sorting definition
          TRY.
              io_sorts->add_sort(
                      EXPORTING
                        iv_column_name        = <ls_sort>-fieldname
                        iv_position           = sy-tabix
                        iv_sequence           = COND #( WHEN <ls_sort>-down = abap_true THEN
                                                          zif_uitb_c_alv_sorting=>descending
                                                        ELSE
                                                          zif_uitb_c_alv_sorting=>ascending  )
                        if_subtotal           = <ls_sort>-subtot
                    ).
            CATCH zcx_uitb_alv_error ##no_handler.
              CONTINUE.
          ENDTRY.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_variant.
    CHECK ir_layout IS BOUND.

    cs_variant-report    = ir_layout->ms_key-report.
    cs_variant-handle    = ir_layout->ms_key-handle.
    cs_variant-log_group = ir_layout->ms_key-logical_group.
    cs_variant-variant   = ir_layout->mv_initial_layout.
  ENDMETHOD.


  METHOD get_variant_default.
    CHECK ir_layout IS BOUND.

    rf_default = ir_layout->mf_use_default.
  ENDMETHOD.


  METHOD get_variant_save.
    CHECK ir_layout IS BOUND.

    rv_save = SWITCH #(
      ir_layout->mv_save_restriction
      WHEN zif_uitb_c_alv_layout_restrict=>restrict_cross_user    THEN 'X'
      WHEN zif_uitb_c_alv_layout_restrict=>restrict_user_specific THEN 'U'
      WHEN zif_uitb_c_alv_layout_restrict=>restrict_none          THEN 'A'
      ELSE                                                             'A'
    ).
  ENDMETHOD.


  METHOD set_alv_layout.
    IF ir_columns IS BOUND.
      cs_layout-info_fname = ir_columns->mv_color_column_simple.
      cs_layout-stylefname = ir_columns->mv_style_column.
      cs_layout-ctab_fname = ir_columns->mv_color_column.
      cs_layout-sgl_clk_hd = ir_columns->mf_single_click_sort.
      cs_layout-cwidth_opt = ir_columns->is_optimized( ).
      cs_layout-no_keyfix  = xsdbool( NOT ir_columns->is_key_fixation( ) ).
      cs_layout-no_headers = xsdbool( ir_columns->mf_col_header_visible = abap_false ).
    ENDIF.

    IF ir_display_settings IS BOUND.
      cs_layout-no_rowins = ir_display_settings->mf_no_row_insertions.
      cs_layout-no_toolbar = ir_display_settings->mf_hide_toolbar.
      cs_layout-no_rowmove = ir_display_settings->mf_no_row_moves.
      cs_layout-no_rowmark = ir_display_settings->mf_no_row_marks.
      cs_layout-zebra = ir_display_settings->mf_striped.
      cs_layout-grid_title = ir_display_settings->mv_title.
      cs_layout-smalltitle = ir_display_settings->mf_smalltitle.
      cs_layout-s_dragdrop = ir_display_settings->ms_dnd.
    ENDIF.

    IF ir_selections IS BOUND.
      CASE ir_selections->get_mode( ).
        WHEN zif_uitb_c_alv_selection=>single.
          cs_layout-sel_mode = 'B'.
        WHEN zif_uitb_c_alv_selection=>multiple.
          cs_layout-sel_mode = 'C'.
        WHEN zif_uitb_c_alv_selection=>cell.
          cs_layout-sel_mode = 'D'.
        WHEN zif_uitb_c_alv_selection=>row_column.
          cs_layout-sel_mode = 'A'.
      ENDCASE.
    ENDIF.

  ENDMETHOD.


  METHOD set_current_cell.
***    CHECK cl_salv_model=>is_offline( ) EQ false.

    CHECK ir_grid IS BOUND.

    "Current cell must not be deleted, even it is set on a subtotal/total line
    DATA(ls_roid) = VALUE lvc_s_roid( row_id = ir_selections->ms_current_cell-row ).
    DATA(ls_col) = VALUE lvc_s_col( fieldname = ir_selections->ms_current_cell-column ).

    ir_grid->set_current_cell_via_id(
      is_row_no    = ls_roid
      is_column_id = ls_col
    ).
  ENDMETHOD.


  METHOD set_f4_registrations.
    DATA(lr_iterator) = ir_columns->zif_uitb_list~get_iterator( ).

    WHILE lr_iterator->has_next( ).
      DATA(lr_column) = CAST zcl_uitb_alv_column( lr_iterator->get_next( ) ).
      CHECK lr_column->has_custom_f4( ).

      rt_f4 = VALUE #( BASE rt_f4
        ( fieldname  = lr_column->get_name( )
          getbefore  = abap_true
          chngeafter = abap_true
          internal   = abap_false
          register   = abap_true  )
      ).

    ENDWHILE.

  ENDMETHOD.


  METHOD set_fieldcatalog.
    DATA(lv_colheader_mode) = ir_columns->get_column_header_mode( ).

    DATA(lr_iterator) = ir_columns->zif_uitb_list~get_iterator( ).

    WHILE lr_iterator->has_next( ).
      DATA(lr_column) = CAST zcl_uitb_alv_column( lr_iterator->get_next( ) ).

      DATA(ls_fcat) = lr_column->to_structure( ).

      ls_fcat-col_pos = sy-index.

      IF NOT ir_display_settings->is_editable( ).
        IF lr_column->get_cell_type( ) = zif_uitb_c_alv_cell_types=>button.
          CLEAR ls_fcat-style.
        ELSEIF lr_column->get_cell_type( ) = zif_uitb_c_alv_cell_types=>checkbox OR
              lr_column->get_cell_type( ) = zif_uitb_c_alv_cell_types=>checkbox_hotspot.
          ls_fcat-edit = abap_false.
        ENDIF.
      ENDIF.

      CASE lv_colheader_mode.

          " nothing to do
        WHEN zif_uitb_c_alv_colheader_mode=>default.

        WHEN zif_uitb_c_alv_colheader_mode=>tech_as_column_header.
          ls_fcat-reptext =
          ls_fcat-coltext = ls_fcat-fieldname.

        WHEN zif_uitb_c_alv_colheader_mode=>tech_as_tooltip.
          ls_fcat-tooltip = ls_fcat-fieldname.
      ENDCASE.

      rt_fieldcat = VALUE #( BASE rt_fieldcat ( ls_fcat ) ).
    ENDWHILE.
  ENDMETHOD.


  METHOD set_filters.
    DATA(lr_filter_enumerator) = ir_filters->zif_uitb_enumerable~get_enumerator( ).

    WHILE lr_filter_enumerator->has_next( ).
      DATA(lr_filter) = CAST zcl_uitb_alv_filter( lr_filter_enumerator->get_next( ) ).

      TRY.
          DATA(lr_column) = ir_filters->mr_columns->get_column( iv_columnname = lr_filter->get_columnname( ) ).

          LOOP AT lr_filter->get( ) ASSIGNING FIELD-SYMBOL(<ls_filter_selopt>).
            rt_filters = VALUE #(
              BASE rt_filters
              ( fieldname         = lr_filter->get_columnname( )
                datatype          = lr_column->ms_data-datatype
                inttype           = lr_column->ms_data-inttype
                intlen            = lr_column->ms_data-intlen
                decimals          = lr_column->ms_data-decimals
                ref_table         = lr_column->ms_data-ref_table
                ref_field         = lr_column->ms_data-ref_field
                edit_mask         = lr_column->ms_data-edit_mask
                lowercase         = lr_column->ms_data-lowercase
                seltext           = lr_column->ms_data-scrtext_l
                sign              = <ls_filter_selopt>-sign
                option            = <ls_filter_selopt>-option
                low               = <ls_filter_selopt>-low
                valuf             = <ls_filter_selopt>-low
                high              = <ls_filter_selopt>-high
                valut             = <ls_filter_selopt>-high )
             ).
          ENDLOOP.
        CATCH cx_sy_itab_line_not_found.
          CONTINUE.
      ENDTRY.
    ENDWHILE.

  ENDMETHOD.


  METHOD set_selected_cells.
    DATA:
      lt_lvc_cell TYPE lvc_t_cell,
      ls_lvc_cell TYPE lvc_s_cell,
      ls_cell     TYPE salv_s_cell.

***    CHECK cl_salv_model=>is_offline( ) EQ false.

    CHECK ir_grid IS BOUND.

    DATA(lt_cells) = VALUE lvc_t_cell(
        FOR <ls_alv_cell> IN ir_selections->mt_selected_cells
        ( row_id-index     = <ls_alv_cell>-row
          col_id-fieldname = <ls_alv_cell>-column )
    ).

    ir_grid->set_selected_cells(
      it_cells = lt_cells
    ).

  ENDMETHOD.


  METHOD set_selected_cols.

***    CHECK cl_salv_model=>is_offline( ) EQ false.

    CHECK ir_grid IS BOUND.

    DATA(lt_col) = VALUE lvc_t_col(
      FOR <lv_col> IN ir_selections->mt_selected_columns
      ( fieldname = <lv_col> )
    ).

    ir_grid->set_selected_columns(
      it_col_table             = lt_col
      is_keep_other_selections = abap_true
    ).
  ENDMETHOD.


  METHOD set_selected_rows.
***    CHECK cl_salv_model=>is_offline( ) EQ false.

    CHECK ir_grid IS BOUND.

    DATA(lt_row) = VALUE lvc_t_row(
      FOR <lv_row> IN ir_selections->mt_selected_rows
      ( index = <lv_row> )
    ).

    ir_grid->set_selected_rows(
      it_index_rows            = lt_row
      is_keep_other_selections = abap_true
    ).
  ENDMETHOD.


  METHOD set_sorting.
    DATA(lr_sort_enumerator) = ir_sorts->zif_uitb_enumerable~get_enumerator( ).

    WHILE lr_sort_enumerator->has_next( ).
      DATA(lr_sort) = CAST zcl_uitb_alv_sort( lr_sort_enumerator->get_next( ) ).

      DATA(ls_sort) = VALUE lvc_s_sort(
        fieldname = lr_sort->get_column_name( )
        subtot    = lr_sort->mf_subtotal
        spos      = sy-tabix
      ).

      CASE lr_sort->get_sequence( ).

        WHEN zif_uitb_c_alv_sorting=>ascending.
          ls_sort-up = abap_true.

        WHEN zif_uitb_c_alv_sorting=>descending.
          ls_sort-down = abap_false.
      ENDCASE.

      result = VALUE #(
        BASE result
        ( ls_sort )
      ).
    ENDWHILE.
  ENDMETHOD.


  METHOD set_variant.
    CHECK io_layout IS BOUND.

    io_layout->ms_key-report          = is_variant-report.
    io_layout->ms_key-handle          = is_variant-handle.
    io_layout->ms_key-logical_group   = is_variant-log_group.
    io_layout->ms_current_layout-layout = is_variant-variant.
    IF is_variant-username IS INITIAL.
      io_layout->ms_current_layout-user_specific = abap_false.
    ELSE.
      io_layout->ms_current_layout-user_specific = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
