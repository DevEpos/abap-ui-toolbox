CLASS ZCL_UITB_alv_controller DEFINITION
  PUBLIC
  CREATE PUBLIC
  GLOBAL FRIENDS ZCL_UITB_alv_grid_adapter.

  PUBLIC SECTION.
    INTERFACES ZIF_UITB_alv_metadata_ctrller.
    INTERFACES ZIF_UITB_alv_controller.

    METHODS constructor
      IMPORTING
        ir_model TYPE REF TO ZCL_UITB_alv.
    DATA mr_model TYPE REF TO ZCL_UITB_alv.
    DATA mr_adapter TYPE REF TO ZCL_UITB_alv_grid_adapter.
    DATA mf_event_mode TYPE abap_bool.

    METHODS display.
    METHODS set_function
      IMPORTING
        iv_function TYPE ui_func.
    METHODS refresh
      IMPORTING
        is_stable               TYPE lvc_s_stbl OPTIONAL
        if_keep_scroll_position TYPE abap_bool OPTIONAL.
    METHODS focus.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mt_changelist TYPE ZIF_UITB_alv_types=>tt_alv_changelist.
ENDCLASS.



CLASS ZCL_UITB_alv_controller IMPLEMENTATION.

  METHOD display.
    IF mr_adapter IS INITIAL.
      mr_adapter = NEW ZCL_UITB_alv_grid_adapter(
          ir_controller = me
      ).
    ENDIF.
    refresh( ).
  ENDMETHOD.

  METHOD refresh.
    TRY.
        DATA: ls_stable TYPE lvc_s_stbl.

        IF mr_adapter IS BOUND.
          IF mf_event_mode EQ abap_true.
            mr_adapter->ms_stable = ls_stable.
            mr_adapter->mf_keep_scroll_position = if_keep_scroll_position.
            RETURN.
          ELSE. " before and after event
            " continue processing coding
          ENDIF.
        ELSE. " adapter not bound
          RETURN.
        ENDIF.

        CHECK ZIF_UITB_alv_metadata_ctrller~check_changelist( ) NE abap_false.

        DATA(lr_changelist) = ZCL_UITB_alv_changelist=>create( mt_changelist ).

        DELETE lr_changelist->mt_changelist
          WHERE flavour NE if_salv_c_changelist_flavour=>setter
            AND flavour NE if_salv_c_changelist_flavour=>data_set
            AND flavour NE if_salv_c_changelist_flavour=>application
            AND flavour NE if_salv_c_changelist_flavour=>selections
            AND flavour NE if_salv_c_changelist_flavour=>close_screen
            AND flavour NE if_salv_c_changelist_flavour=>functions
            AND flavour NE if_salv_c_changelist_flavour=>refresh.

        " new call --> therefore setters and application irrelevant

        ZIF_UITB_alv_metadata_ctrller~clear_changelist( iv_flavour = if_salv_c_changelist_flavour=>setter ).
        ZIF_UITB_alv_metadata_ctrller~clear_changelist( iv_flavour = if_salv_c_changelist_flavour=>data_set ).
        ZIF_UITB_alv_metadata_ctrller~clear_changelist( iv_flavour = if_salv_c_changelist_flavour=>application ).
        ZIF_UITB_alv_metadata_ctrller~clear_changelist( iv_flavour = if_salv_c_changelist_flavour=>refresh ).

        CHECK lr_changelist->is_new_data_requested( ) OR
              lr_changelist->has_metadata_changed( ) OR
              lr_changelist->is_refresh_requested( )  OR
              lr_changelist->is_functions_change_requested( ) OR
              lr_changelist->is_selections_requested( ).
*              lr_changelist->is_screen_closed( ) .

        " send the data to the frontend
        mr_adapter->set_metadata(
          is_stable               = is_stable
          if_keep_scroll_position = if_keep_scroll_position
          ir_changelist           = lr_changelist
        ).
      CATCH ZCX_UITB_alv_error.
    ENDTRY.
  ENDMETHOD.

  METHOD constructor.
    mr_model = ir_model.
  ENDMETHOD.

  METHOD ZIF_UITB_alv_metadata_ctrller~register.

    CHECK line_exists(
            mt_changelist[
                name    = iv_name
                flavour = zif_uitb_c_alv_chglist_flavor=>register ] ).

    INSERT VALUE #(
        name    = iv_name
        flavour = zif_uitb_c_alv_chglist_flavor=>register
        ref     = ir_ref
    ) INTO TABLE mt_changelist.
  ENDMETHOD.

  METHOD ZIF_UITB_alv_metadata_ctrller~set_changed.

    DATA: ls_changelist TYPE ZIF_UITB_alv_types=>ty_alv_s_changelist.

    " check if object has been registered
    IF NOT line_exists(
              mt_changelist[
                name    = iv_name
                flavour = zif_uitb_c_alv_chglist_flavor=>register ] ).
      " Exception for ALV
    ENDIF.

    DATA(lv_index_of_change) = line_index(
      mt_changelist[
        name    = iv_name
        flavour = iv_flavour
        object  = iv_object
        method  = iv_method
      ]
    ).
    IF lv_index_of_change <> 0.
      DELETE mt_changelist INDEX lv_index_of_change.
    ENDIF.

    IF iv_flavour EQ zif_uitb_c_alv_chglist_flavor=>selections.
      DATA(lt_changelist) = ZIF_UITB_alv_metadata_ctrller~get_changelist( zif_uitb_c_alv_chglist_flavor=>selections ).
      IF lt_changelist IS NOT INITIAL.
        SORT lt_changelist BY sequence DESCENDING.
        ls_changelist = lt_changelist[ 1 ].
        ADD 1 TO ls_changelist-sequence.
      ENDIF.
    ENDIF.

    ls_changelist-name         = iv_name.
    ls_changelist-flavour      = iv_flavour.
    ls_changelist-object       = iv_object.
    ls_changelist-method       = iv_method.
    ls_changelist-change       = abap_true.
    ls_changelist-refresh_mode = iv_refresh_mode.
    ls_changelist-ref          = ir_ref.
    ls_changelist-frontend     = iv_frontend.

    INSERT ls_changelist INTO TABLE mt_changelist.
  ENDMETHOD.

  METHOD ZIF_UITB_alv_metadata_ctrller~clear_changelist.
    DATA: ls_changelist TYPE ZIF_UITB_alv_types=>ty_alv_s_changelist.

    IF iv_name IS SUPPLIED.
      DELETE mt_changelist WHERE name EQ iv_name.
    ENDIF.

    IF iv_object IS SUPPLIED.
      DELETE mt_changelist WHERE object EQ iv_object.
    ENDIF.

    IF iv_method IS SUPPLIED.
      DELETE mt_changelist WHERE method EQ iv_method.
    ENDIF.

    IF if_change IS SUPPLIED.
      DELETE mt_changelist WHERE change EQ if_change.
    ENDIF.

    IF iv_flavour IS SUPPLIED.
      DELETE mt_changelist WHERE flavour EQ iv_flavour.
    ENDIF.

    IF iv_refresh_mode IS SUPPLIED.
      DELETE mt_changelist WHERE refresh_mode EQ iv_refresh_mode.
    ENDIF.

    IF it_range IS SUPPLIED.
      LOOP AT mt_changelist INTO ls_changelist.
        CHECK ls_changelist IN it_range.
        DELETE mt_changelist INDEX sy-tabix.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.

  METHOD ZIF_UITB_alv_metadata_ctrller~check_changelist.
    DATA: ls_changelist TYPE ZIF_UITB_alv_types=>ty_alv_s_changelist.


    IF iv_flavour IS SUPPLIED.
      IF line_exists( mt_changelist[ flavour = iv_flavour ] ).
        rf_exists = abap_true.
      ELSE.
        rf_exists = abap_false.
        EXIT.
      ENDIF.
    ENDIF.

    IF iv_name IS SUPPLIED.
      IF line_exists( mt_changelist[ name = iv_name ] ).
        rf_exists = abap_true.
      ELSE.
        rf_exists = abap_false.
        EXIT.
      ENDIF.
    ENDIF.


    IF iv_object IS SUPPLIED.
      IF line_exists( mt_changelist[ object = iv_object ] ).
        rf_exists = abap_true.
      ELSE.
        rf_exists = abap_false.
        EXIT.
      ENDIF.
    ENDIF.

    IF iv_method IS SUPPLIED.
      IF line_exists( mt_changelist[ method = iv_method ] ).
        rf_exists = abap_true.
      ELSE.
        rf_exists = abap_false.
        EXIT.
      ENDIF.
    ENDIF.

    IF if_change IS SUPPLIED.
      IF line_exists( mt_changelist[ change = if_change ] ).
        rf_exists = abap_true.
      ELSE.
        rf_exists = abap_false.
        EXIT.
      ENDIF.
    ENDIF.

    IF iv_refresh_mode IS SUPPLIED.
      IF line_exists( mt_changelist[ refresh_mode = iv_refresh_mode ] ).
        rf_exists = abap_true.
      ELSE.
        rf_exists = abap_false.
        EXIT.
      ENDIF.
    ENDIF.

    IF it_range IS SUPPLIED.
      LOOP AT mt_changelist INTO ls_changelist.
        rf_exists = abap_false.
        CHECK ls_changelist IN it_range.
        rf_exists = abap_true.
        EXIT.
      ENDLOOP.
      EXIT.
    ENDIF.

    " default if nothing else is supplied
    IF line_exists( mt_changelist[ change = if_change ] ).
      rf_exists = abap_true.
    ELSE.
      rf_exists = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD ZIF_UITB_alv_metadata_ctrller~get_changelist.

    result = mt_changelist.

    DELETE result WHERE flavour NE iv_flavour.
  ENDMETHOD.

  METHOD set_function.
    check mr_adapter is bound.

    mr_adapter->set_function( iv_function ).
  ENDMETHOD.


  METHOD focus.
    check mr_adapter is bound.

    mr_adapter->set_focus_to_grid( ).
  ENDMETHOD.

ENDCLASS.
