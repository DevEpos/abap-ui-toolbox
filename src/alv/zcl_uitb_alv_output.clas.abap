CLASS ZCL_UITB_ALV_OUTPUT DEFINITION
  PUBLIC
  INHERITING FROM cl_gui_alv_grid
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    EVENTS sorted_fields_changed .

    METHODS constructor
      IMPORTING
        ir_parent TYPE REF TO cl_gui_container.
    METHODS has_selected_columns
      RETURNING
        VALUE(rf_columns_selected) TYPE boolean .
    METHODS execute_user_command
      IMPORTING
        !iv_function_code TYPE sy-ucomm .
    METHODS hide_selected_columns .
    METHODS optimize_columns .
    METHODS set_column_names
      IMPORTING
        if_tech_names TYPE boolean.
    METHODS dispatch
         REDEFINITION .
    METHODS set_sort_criteria
         REDEFINITION .

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING
          e_interactive
          e_object.
    METHODS on_menu_button FOR EVENT menu_button OF cl_gui_alv_grid
      IMPORTING
          e_object
          e_ucomm.
ENDCLASS.



CLASS ZCL_UITB_ALV_OUTPUT IMPLEMENTATION.


  METHOD constructor.
    super->constructor(
        i_parent        = ir_parent
        i_fcat_complete = abap_true
    ).

*    CHECK sy-uname = 'STOCKBAL'.
*
*    SET HANDLER:
*        on_toolbar FOR me,
*        on_menu_button FOR me.

  ENDMETHOD.


  METHOD dispatch.
*&---------------------------------------------------------------------*
*& Author:    stockbal     Date: 2016/12/01
*&---------------------------------------------------------------------*
*& Description: Redefined because of additional event handling
*&---------------------------------------------------------------------*

    DATA: lv_action TYPE sy-ucomm.

    get_event_parameter( EXPORTING parameter_id = 0
                                   queue_only   = space
                         IMPORTING parameter    = lv_action ).

    super->dispatch(
      EXPORTING
        cargo             = cargo
        eventid           = eventid
        is_shellevent     = is_shellevent
        is_systemdispatch = is_systemdispatch
      EXCEPTIONS
        cntl_error        = 1
        OTHERS            = 2
    ).
    IF sy-subrc <> 0.
* Implement suitable error handling here
      RETURN.
    ENDIF.

    " handle specific function codes
    CASE lv_action.
      WHEN mc_mb_variant OR
           mc_fc_current_variant OR
           mc_fc_load_variant OR
           mc_fc_save_variant OR
           mc_fc_maintain_variant OR
           mc_fc_variant_admin OR
           mc_fc_sort OR
           mc_fc_sort_asc OR
           mc_fc_sort_dsc.
        IF eventid <> evt_toolbar_menubutton_click.
          RAISE EVENT sorted_fields_changed.

          " refresh the table
          refresh_table_display( is_stable = VALUE #( row = abap_true col = abap_true )
                                        i_soft_refresh = abap_true ).
        ENDIF.
    ENDCASE.
  ENDMETHOD.


  METHOD execute_user_command.
    fcode_bouncer( ).
  ENDMETHOD.


  METHOD has_selected_columns.
    get_selected_columns( IMPORTING et_index_columns = DATA(lt_col_index) ).

    rf_columns_selected = xsdbool( lt_col_index IS NOT INITIAL ).
  ENDMETHOD.


  METHOD hide_selected_columns.

    get_scroll_info_via_id( IMPORTING es_row_info = DATA(ls_row_id)
                                      es_col_info = DATA(ls_col_id) ).

    get_selected_columns( IMPORTING et_index_columns = DATA(lt_index_columns) ).

    IF me->is_cache_valid( ) NE abap_true OR www_active EQ abap_true.
      cl_gui_cfw=>flush( ).
    ENDIF.

    get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(lt_fieldcat) ).

    IF lt_index_columns IS NOT INITIAL.
      LOOP AT lt_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fieldcat>).
        IF line_exists( lt_index_columns[ fieldname = <ls_fieldcat>-fieldname ] ).
          <ls_fieldcat>-no_out = 'X'.
        ENDIF.
      ENDLOOP.

      " update fieldcatalog
      set_frontend_fieldcatalog( lt_fieldcat ).

      set_scroll_info_via_id(
          is_row_info = ls_row_id
          is_col_info = ls_col_id
      ).
      refresh_table_display( ).
    ELSE.
      MESSAGE s005(0k).
    ENDIF.
  ENDMETHOD.


  METHOD on_menu_button.

  ENDMETHOD.


  METHOD on_toolbar.
    CLEAR e_object->mt_toolbar.



    e_object->mt_toolbar = VALUE #(
      ( function  = 'HDR_VSBLTY'
        icon      = icon_overview
        quickinfo = 'Zusatzinfos ein-/ausblenden' )
      ( butn_type = cntb_btype_sep )
      ( function  = 'REFRESH'
        icon      = icon_refresh
        quickinfo = 'Aktualisieren' )
      ( butn_type = cntb_btype_sep )
      ( function  = '&ALL'
        icon      = icon_select_all
        quickinfo = 'Alle Zeilen selektieren' )
      ( function  = '&SAL'
        icon      = icon_deselect_all
        quickinfo = 'Alle Zeilen deselektieren' )
      ( butn_type = cntb_btype_sep )
      ( function  = '&OUP'
        icon      = icon_sort_up
        quickinfo = 'Aufsteigend sortieren' )
      ( function  = '&ODN'
        icon      = icon_sort_down
        quickinfo = 'Absteigend sortieren' )
      ( butn_type = cntb_btype_dropdown
        function  = 'DRPDN'
        text      = 'Drop'
        )
    ).
  ENDMETHOD.


  METHOD optimize_columns.
    optimize_all_cols(
*      EXPORTING
*        include_header = 1    " Spaltenüberschriften berücksichtigen (0=Nein, 1=Ja)
*      EXCEPTIONS
*        error          = 1
*        others         = 2
    ).
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD set_column_names.
    DATA: lv_tooltip TYPE lvc_tip,
          lv_coltext TYPE lvc_txtcol.

    get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(lt_fcat) ).

    LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).
      DATA(ls_fcat) = <ls_fcat>.
      <ls_fcat>-tooltip = ls_fcat-coltext.
      <ls_fcat>-coltext = ls_fcat-tooltip.
    ENDLOOP.

    set_frontend_fieldcatalog( lt_fcat ).

    refresh_table_display( is_stable = VALUE #( row = abap_true col = abap_true ) ).
  ENDMETHOD.


  METHOD set_sort_criteria.
*&---------------------------------------------------------------------*
*& Author:    stockbal     Date: 2016/12/01
*&---------------------------------------------------------------------*
    super->set_sort_criteria( EXPORTING it_sort = it_sort ).

    RAISE EVENT sorted_fields_changed.
  ENDMETHOD.
ENDCLASS.
