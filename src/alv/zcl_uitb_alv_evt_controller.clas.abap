class ZCL_UITB_ALV_EVT_CONTROLLER definition
  public
  abstract
  final
  create public .

public section.

  interfaces ZIF_UITB_ALV_CONTROLLER .

  class-methods RAISE_F4
    importing
      !IR_CONTROLLER type ref to ZCL_UITB_ALV_CONTROLLER
      !IV_FIELDNAME type LVC_FNAME
      !IV_FIELDVALUE type LVC_VALUE
      !IS_ROW_NO type LVC_S_ROID
      !IR_EVENT_DATA type ref to CL_ALV_EVENT_DATA
      !IT_BAD_CELLS type LVC_T_MODI optional
      !IF_DISPLAY type ABAP_BOOL optional .
  class-methods RAISE_FUNCTION_CHOSEN
    importing
      !IR_CONTROLLER type ref to ZCL_UITB_ALV_CONTROLLER
      !IV_FUNCTION type UI_FUNC
      !IV_TAG type STRING optional .
  class-methods RAISE_DOUBLE_CLICK
    importing
      !IR_CONTROLLER type ref to ZCL_UITB_ALV_CONTROLLER
      !IV_ROW type I
      !IV_COLUMN type LVC_FNAME .
  class-methods RAISE_LINK_CLICK
    importing
      !IR_CONTROLLER type ref to ZCL_UITB_ALV_CONTROLLER
      !IV_ROW type I
      !IV_COLUMN type LVC_FNAME .
  class-methods RAISE_BEFORE_FUNCTION
    importing
      !IR_CONTROLLER type ref to ZCL_UITB_ALV_CONTROLLER
      !IV_FUNCTION type UI_FUNC .
  class-methods RAISE_AFTER_FUNCTION
    importing
      !IR_CONTROLLER type ref to ZCL_UITB_ALV_CONTROLLER
      !IV_FUNCTION type UI_FUNC .
  class-methods HANDLE_AFTER_EVENT
    importing
      !IR_CONTROLLER type ref to ZCL_UITB_ALV_CONTROLLER .
  class-methods HANDLE_BEFORE_EVENT
    importing
      !IR_CONTROLLER type ref to ZCL_UITB_ALV_CONTROLLER .
  class-methods RAISE_DATA_CHANGED
    importing
      !IR_CONTROLLER type ref to ZCL_UITB_ALV_CONTROLLER
      !IR_CHANGE_PROTOCOL type ref to CL_ALV_CHANGED_DATA_PROTOCOL optional
      !IF_ONF4 type ABAP_BOOL optional
      !IF_ONF4_BEFORE type ABAP_BOOL optional
      !IF_ONF4_AFTER type ABAP_BOOL optional
      !IV_FUNCTION type SY-UCOMM optional .
  class-methods RAISE_DROP_GET_FLAVOR
    importing
      !IR_CONTROLLER type ref to ZCL_UITB_ALV_CONTROLLER
      !IV_ROW type I
      !IV_COLUMN type LVC_FNAME
      !IS_ROW_NO type LVC_S_ROID
      !IR_DRAGDROPOBJ type ref to CL_DRAGDROPOBJECT
      !IT_FLAVORS type CNDD_FLAVORS optional .
  class-methods RAISE_DRAG
    importing
      !IR_CONTROLLER type ref to ZCL_UITB_ALV_CONTROLLER
      !IV_ROW type I
      !IV_COLUMN type LVC_FNAME
      !IS_ROW_NO type LVC_S_ROID
      !IR_DRAGDROPOBJ type ref to CL_DRAGDROPOBJECT optional .
  class-methods RAISE_DROP
    importing
      !IR_CONTROLLER type ref to ZCL_UITB_ALV_CONTROLLER
      !IV_ROW type I
      !IV_COLUMN type LVC_FNAME
      !IS_ROW_NO type LVC_S_ROID
      !IR_DRAGDROPOBJ type ref to CL_DRAGDROPOBJECT optional .
  class-methods RAISE_DROP_COMPLETE
    importing
      !IR_CONTROLLER type ref to ZCL_UITB_ALV_CONTROLLER
      !IV_ROW type I
      !IV_COLUMN type LVC_FNAME
      !IS_ROW_NO type LVC_S_ROID
      !IR_DRAGDROPOBJ type ref to CL_DRAGDROPOBJECT optional .
  class-methods RAISE_CONTEXT_MENU
    importing
      !IR_CONTROLLER type ref to ZCL_UITB_ALV_CONTROLLER
      !IR_MENU type ref to CL_CTMENU .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS get_events
      IMPORTING
        ir_controller    TYPE REF TO zcl_uitb_alv_controller
      RETURNING
        VALUE(rr_events) TYPE REF TO zcl_uitb_alv_events.

ENDCLASS.



CLASS ZCL_UITB_ALV_EVT_CONTROLLER IMPLEMENTATION.


  METHOD get_events.
    DATA(lr_model) = ir_controller->mr_model.
    rr_events = lr_model->get_events( ).
  ENDMETHOD.


  METHOD handle_after_event .

    IF ir_controller->mr_adapter IS NOT BOUND.
      RETURN.
    ENDIF.

    DATA(ls_stable) = ir_controller->mr_adapter->ms_stable.
    DATA(lf_keep_scroll_positioon) = ir_controller->mr_adapter->mf_keep_scroll_position.
    ir_controller->mf_event_mode = abap_false.

    ir_controller->refresh(
      is_stable               = ls_stable
      if_keep_scroll_position = lf_keep_scroll_positioon
    ).

  ENDMETHOD.


  METHOD handle_before_event .

    IF ir_controller->mr_adapter IS NOT BOUND.
      RETURN.
    ENDIF.

    ir_controller->mf_event_mode = abap_true.

    ir_controller->mr_adapter->get_metadata( ).

    ir_controller->zif_uitb_alv_metadata_ctrller~clear_changelist( iv_flavour = zif_uitb_c_alv_chglist_flavor=>setter ).
    ir_controller->zif_uitb_alv_metadata_ctrller~clear_changelist( iv_flavour = zif_uitb_c_alv_chglist_flavor=>selections ).
    ir_controller->zif_uitb_alv_metadata_ctrller~clear_changelist( iv_flavour = zif_uitb_c_alv_chglist_flavor=>functions ).

  ENDMETHOD.


  METHOD raise_after_function.
    DATA: lv_evts TYPE i.

    DATA(lr_events) = get_events( ir_controller ).
    IF lr_events IS NOT BOUND.
      RETURN.
    ENDIF.


    SYSTEM-CALL EVENTS GET NUM_HANDLERS
      FOR after_function OF INST lr_events INTO lv_evts.

    IF NOT lv_evts > 0.
      RETURN.
    ENDIF.

    handle_before_event( ir_controller ).

    lr_events->raise_after_function(
      iv_function = iv_function
    ).

    handle_after_event( ir_controller ).
  ENDMETHOD.


  METHOD raise_before_function.
    DATA: lv_evts TYPE i.

    DATA(lr_events) = get_events( ir_controller ).
    IF lr_events IS NOT BOUND.
      RETURN.
    ENDIF.


    SYSTEM-CALL EVENTS GET NUM_HANDLERS
      FOR before_function OF INST lr_events INTO lv_evts.

    IF NOT lv_evts > 0.
      RETURN.
    ENDIF.

    handle_before_event( ir_controller ).

    lr_events->raise_before_function(
      iv_function = iv_function
    ).

    handle_after_event( ir_controller ).
  ENDMETHOD.


  METHOD raise_context_menu.
    DATA: lv_evts TYPE i.

    DATA(lr_events) = get_events( ir_controller ).
    IF lr_events IS NOT BOUND.
      RETURN.
    ENDIF.


    SYSTEM-CALL EVENTS GET NUM_HANDLERS
      FOR context_menu OF INST lr_events INTO lv_evts.

    IF NOT lv_evts > 0.
      RETURN.
    ENDIF.

    handle_before_event( ir_controller ).

    lr_events->raise_context_menu(
      ir_menu
    ).

    handle_after_event( ir_controller ).

  ENDMETHOD.


  METHOD raise_data_changed.
    DATA: lv_evts TYPE i.

    DATA(lr_events) = get_events( ir_controller ).
    IF lr_events IS NOT BOUND.
      RETURN.
    ENDIF.


    SYSTEM-CALL EVENTS GET NUM_HANDLERS
      FOR data_changed OF INST lr_events INTO lv_evts.

    IF NOT lv_evts > 0.
      RETURN.
    ENDIF.

    handle_before_event( ir_controller ).

    lr_events->raise_data_changed(
      ir_change_protocol = ir_change_protocol
      if_onf4            = if_onf4
      if_onf4_before     = if_onf4_before
      if_onf4_after      = if_onf4_after
      iv_function        = iv_function
    ).

    " @TODO: check if causes an error, because refresh can normally not be fired during data_changed event of alv
    handle_after_event( ir_controller ).
  ENDMETHOD.


  METHOD raise_double_click.
    DATA: lv_evts TYPE i.

    DATA(lr_events) = get_events( ir_controller ).
    IF lr_events IS NOT BOUND.
      RETURN.
    ENDIF.


    SYSTEM-CALL EVENTS GET NUM_HANDLERS
      FOR double_click OF INST lr_events INTO lv_evts.

    IF NOT lv_evts > 0.
      RETURN.
    ENDIF.

    handle_before_event( ir_controller ).

    lr_events->raise_double_click(
      iv_row    = iv_row
      iv_column = iv_column
    ).

    handle_after_event( ir_controller ).
  ENDMETHOD.


  METHOD raise_drag.
    DATA: lv_evts TYPE i.

    DATA(lr_events) = get_events( ir_controller ).
    IF lr_events IS NOT BOUND.
      RETURN.
    ENDIF.


    SYSTEM-CALL EVENTS GET NUM_HANDLERS
      FOR drag OF INST lr_events INTO lv_evts.

    IF NOT lv_evts > 0.
      RETURN.
    ENDIF.

    handle_before_event( ir_controller ).

    lr_events->raise_drag(
        iv_row         = iv_row
        iv_column      = iv_column
        is_row_no      = is_row_no
        ir_dragdropobj = ir_dragdropobj
    ).

    handle_after_event( ir_controller ).
  ENDMETHOD.


  METHOD raise_drop.
    DATA: lv_evts TYPE i.

    DATA(lr_events) = get_events( ir_controller ).
    IF lr_events IS NOT BOUND.
      RETURN.
    ENDIF.


    SYSTEM-CALL EVENTS GET NUM_HANDLERS
      FOR drop OF INST lr_events INTO lv_evts.

    IF NOT lv_evts > 0.
      RETURN.
    ENDIF.

    handle_before_event( ir_controller ).

    lr_events->raise_drop(
        iv_row         = iv_row
        iv_column      = iv_column
        is_row_no      = is_row_no
        ir_dragdropobj = ir_dragdropobj
    ).

    handle_after_event( ir_controller ).
  ENDMETHOD.


  METHOD raise_drop_complete.
    DATA: lv_evts TYPE i.

    DATA(lr_events) = get_events( ir_controller ).
    IF lr_events IS NOT BOUND.
      RETURN.
    ENDIF.


    SYSTEM-CALL EVENTS GET NUM_HANDLERS
      FOR drop_complete OF INST lr_events INTO lv_evts.

    IF NOT lv_evts > 0.
      RETURN.
    ENDIF.

    handle_before_event( ir_controller ).

    lr_events->raise_drop_complete(
        iv_row         = iv_row
        iv_column      = iv_column
        is_row_no      = is_row_no
        ir_dragdropobj = ir_dragdropobj
    ).

    handle_after_event( ir_controller ).
  ENDMETHOD.


  METHOD raise_drop_get_flavor.
    DATA: lv_evts TYPE i.

    DATA(lr_events) = get_events( ir_controller ).
    IF lr_events IS NOT BOUND.
      RETURN.
    ENDIF.


    SYSTEM-CALL EVENTS GET NUM_HANDLERS
      FOR drop_get_flavor OF INST lr_events INTO lv_evts.

    IF NOT lv_evts > 0.
      RETURN.
    ENDIF.

    handle_before_event( ir_controller ).

    lr_events->raise_drop_get_flavor(
        iv_row         = iv_row
        iv_column      = iv_column
        is_row_no      = is_row_no
        ir_dragdropobj = ir_dragdropobj
        it_flavors     = it_flavors
    ).

    handle_after_event( ir_controller ).
  ENDMETHOD.


  METHOD raise_f4.
    DATA: lv_evts TYPE i.

    DATA(lr_events) = get_events( ir_controller ).
    IF lr_events IS NOT BOUND.
      RETURN.
    ENDIF.


    SYSTEM-CALL EVENTS GET NUM_HANDLERS
      FOR f4 OF INST lr_events INTO lv_evts.

    IF NOT lv_evts > 0.
      RETURN.
    ENDIF.

    handle_before_event( ir_controller ).

    lr_events->raise_f4(
        iv_fieldname  = iv_fieldname
        iv_fieldvalue = iv_fieldvalue
        is_row_no     = is_row_no
        ir_event_data = ir_event_data
        it_bad_cells  = it_bad_cells
        if_display    = if_display
    ).

    " @TODO: check if causes an error because this call will refresh the alv
    handle_after_event( ir_controller ).
  ENDMETHOD.


  METHOD raise_function_chosen.
    DATA: lv_evts TYPE i.

    DATA(lr_events) = get_events( ir_controller ).
    IF lr_events IS NOT BOUND.
      RETURN.
    ENDIF.


    SYSTEM-CALL EVENTS GET NUM_HANDLERS
      FOR function_chosen OF INST lr_events INTO lv_evts.

    IF NOT lv_evts > 0.
      RETURN.
    ENDIF.

    handle_before_event( ir_controller ).

    lr_events->raise_function_chosen(
      iv_function = iv_function
      iv_tag      = iv_tag
    ).

    handle_after_event( ir_controller ).

  ENDMETHOD.


  METHOD raise_link_click.
    DATA: lv_evts TYPE i.

    DATA(lr_events) = get_events( ir_controller ).
    IF lr_events IS NOT BOUND.
      RETURN.
    ENDIF.


    SYSTEM-CALL EVENTS GET NUM_HANDLERS
      FOR link_click OF INST lr_events INTO lv_evts.

    IF NOT lv_evts > 0.
      RETURN.
    ENDIF.

    handle_before_event( ir_controller ).

    lr_events->raise_link_click(
      iv_row     = iv_row
      iv_column  = iv_column
    ).

    handle_after_event( ir_controller ).
  ENDMETHOD.
ENDCLASS.
