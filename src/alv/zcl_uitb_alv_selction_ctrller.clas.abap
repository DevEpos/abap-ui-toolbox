CLASS ZCL_UITB_alv_selction_ctrller DEFINITION
  PUBLIC
  FINAL
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS set_selections
      IMPORTING
        ir_controller            TYPE REF TO ZCL_UITB_alv_controller
      RETURNING
        VALUE(rf_selections_set) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_UITB_alv_selction_ctrller IMPLEMENTATION.
  METHOD set_selections.
    rf_selections_set = abap_false.

    TRY.
        DATA(lr_selections) = CAST ZIF_UITB_alv_adpt_selections( ir_controller->mo_adapter ).

        " we can not sort by frontend as in some languages SPACE is sorted after non-SPACE
        " characters, in it is sorted before non-SPACE characters
        DATA(lt_changelist_frontend) = ir_controller->ZIF_UITB_alv_metadata_ctrller~get_changelist(
            zif_uitb_c_alv_chglist_flavor=>selections
        ).
        DATA(lt_changelist_no_frontend) = lt_changelist_frontend.

        DELETE lt_changelist_no_frontend WHERE frontend EQ abap_true.
        DELETE lt_changelist_frontend WHERE frontend EQ abap_false.

        DATA(lt_changelist) = VALUE ZIF_UITB_alv_types=>tt_alv_changelist(
          ( LINES OF lt_changelist_frontend )
          ( LINES OF lt_changelist_no_frontend )
        ).

        CHECK lt_changelist IS NOT INITIAL.

        rf_selections_set = abap_true.

        SORT lt_changelist BY frontend DESCENDING
                              sequence ASCENDING.

        LOOP AT lt_changelist ASSIGNING FIELD-SYMBOL(<ls_changelist>) WHERE change = abap_true.

          CASE <ls_changelist>-object.

            WHEN zif_uitb_c_alv_selection_type=>cell.
              DATA(lf_set_current_cell) = abap_true.

            WHEN zif_uitb_c_alv_selection_type=>cells.
              IF NOT ( lf_set_current_cell = abap_true AND <ls_changelist>-name EQ 'EXIT' ).
                lr_selections->set_selected_cells( ).
              ENDIF.

            WHEN zif_uitb_c_alv_selection_type=>columns.
              lr_selections->set_selected_columns( ).

            WHEN zif_uitb_c_alv_selection_type=>rows.
              lr_selections->set_selected_rows( ).

          ENDCASE.
        ENDLOOP.

        IF lf_set_current_cell = abap_true.
          lr_selections->set_current_cell( ).
        ENDIF.

        ir_controller->ZIF_UITB_alv_metadata_ctrller~clear_changelist( iv_flavour = zif_uitb_c_alv_chglist_flavor=>selections ).

      CATCH cx_sy_move_cast_error.
        rf_selections_set = abap_false.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
