CLASS zcl_uitb_alv_evt_controller DEFINITION
  PUBLIC
  ABSTRACT
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_uitb_alv_controller .

    CLASS-METHODS raise_f4
      IMPORTING
        !io_controller TYPE REF TO zcl_uitb_alv_controller
        !iv_fieldname  TYPE lvc_fname
        !iv_fieldvalue TYPE lvc_value
        !is_row_no     TYPE lvc_s_roid
        !io_event_data TYPE REF TO cl_alv_event_data
        !it_bad_cells  TYPE lvc_t_modi OPTIONAL
        !if_display    TYPE abap_bool OPTIONAL .
    CLASS-METHODS raise_function_chosen
      IMPORTING
        !io_controller TYPE REF TO zcl_uitb_alv_controller
        !iv_function   TYPE ui_func
        !iv_tag        TYPE string OPTIONAL .
    CLASS-METHODS raise_double_click
      IMPORTING
        !io_controller TYPE REF TO zcl_uitb_alv_controller
        !iv_row        TYPE i
        !iv_column     TYPE lvc_fname .
    CLASS-METHODS raise_link_click
      IMPORTING
        !io_controller TYPE REF TO zcl_uitb_alv_controller
        !iv_row        TYPE i
        !iv_column     TYPE lvc_fname .
    CLASS-METHODS raise_before_function
      IMPORTING
        !io_controller TYPE REF TO zcl_uitb_alv_controller
        !iv_function   TYPE ui_func .
    CLASS-METHODS raise_after_function
      IMPORTING
        !io_controller TYPE REF TO zcl_uitb_alv_controller
        !iv_function   TYPE ui_func .
    CLASS-METHODS handle_after_event
      IMPORTING
        !io_controller TYPE REF TO zcl_uitb_alv_controller .
    CLASS-METHODS handle_before_event
      IMPORTING
        !io_controller TYPE REF TO zcl_uitb_alv_controller .
    CLASS-METHODS raise_data_changed
      IMPORTING
        !io_controller      TYPE REF TO zcl_uitb_alv_controller
        !io_change_protocol TYPE REF TO cl_alv_changed_data_protocol OPTIONAL
        !if_onf4            TYPE abap_bool OPTIONAL
        !if_onf4_before     TYPE abap_bool OPTIONAL
        !if_onf4_after      TYPE abap_bool OPTIONAL
        !iv_function        TYPE sy-ucomm OPTIONAL .
    CLASS-METHODS raise_drop_get_flavor
      IMPORTING
        !io_controller  TYPE REF TO zcl_uitb_alv_controller
        !iv_row         TYPE i
        !iv_column      TYPE lvc_fname
        !is_row_no      TYPE lvc_s_roid
        !io_dragdropobj TYPE REF TO cl_dragdropobject
        !it_flavors     TYPE cndd_flavors OPTIONAL .
    CLASS-METHODS raise_drag
      IMPORTING
        !io_controller  TYPE REF TO zcl_uitb_alv_controller
        !iv_row         TYPE i
        !iv_column      TYPE lvc_fname
        !is_row_no      TYPE lvc_s_roid
        !io_dragdropobj TYPE REF TO cl_dragdropobject OPTIONAL .
    CLASS-METHODS raise_drop
      IMPORTING
        !io_controller  TYPE REF TO zcl_uitb_alv_controller
        !iv_row         TYPE i
        !iv_column      TYPE lvc_fname
        !is_row_no      TYPE lvc_s_roid
        !io_dragdropobj TYPE REF TO cl_dragdropobject OPTIONAL .
    CLASS-METHODS raise_drop_complete
      IMPORTING
        !io_controller  TYPE REF TO zcl_uitb_alv_controller
        !iv_row         TYPE i
        !iv_column      TYPE lvc_fname
        !is_row_no      TYPE lvc_s_roid
        !io_dragdropobj TYPE REF TO cl_dragdropobject OPTIONAL .
    CLASS-METHODS raise_context_menu
      IMPORTING
        !io_controller TYPE REF TO zcl_uitb_alv_controller
        !io_menu       TYPE REF TO cl_ctmenu .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS get_events
      IMPORTING
        io_controller    TYPE REF TO zcl_uitb_alv_controller
      RETURNING
        VALUE(ro_events) TYPE REF TO zcl_uitb_alv_events.

ENDCLASS.



CLASS zcl_uitb_alv_evt_controller IMPLEMENTATION.


  METHOD get_events.
    DATA(lo_model) = io_controller->mo_model.
    ro_events = lo_model->get_events( ).
  ENDMETHOD.


  METHOD handle_after_event .

    IF io_controller->mo_adapter IS NOT BOUND.
      RETURN.
    ENDIF.

    DATA(ls_stable) = io_controller->mo_adapter->ms_stable.
    DATA(lf_keep_scroll_positioon) = io_controller->mo_adapter->mf_keep_scroll_position.
    io_controller->mf_event_mode = abap_false.

    io_controller->refresh(
      is_stable               = ls_stable
      if_keep_scroll_position = lf_keep_scroll_positioon
    ).

  ENDMETHOD.


  METHOD handle_before_event .

    IF io_controller->mo_adapter IS NOT BOUND.
      RETURN.
    ENDIF.

    io_controller->mf_event_mode = abap_true.

    io_controller->mo_adapter->get_metadata( ).

    io_controller->zif_uitb_alv_metadata_ctrller~clear_changelist( iv_flavour = zif_uitb_c_alv_chglist_flavor=>setter ).
    io_controller->zif_uitb_alv_metadata_ctrller~clear_changelist( iv_flavour = zif_uitb_c_alv_chglist_flavor=>selections ).
    io_controller->zif_uitb_alv_metadata_ctrller~clear_changelist( iv_flavour = zif_uitb_c_alv_chglist_flavor=>functions ).

  ENDMETHOD.


  METHOD raise_after_function.
    DATA: lv_evts TYPE i.

    DATA(lo_events) = get_events( io_controller ).
    IF lo_events IS NOT BOUND.
      RETURN.
    ENDIF.


    SYSTEM-CALL EVENTS GET NUM_HANDLERS
      FOR after_function OF INST lo_events INTO lv_evts.

    IF NOT lv_evts > 0.
      RETURN.
    ENDIF.

    handle_before_event( io_controller ).

    lo_events->raise_after_function(
      iv_function = iv_function
    ).

    handle_after_event( io_controller ).
  ENDMETHOD.


  METHOD raise_before_function.
    DATA: lv_evts TYPE i.

    DATA(lo_events) = get_events( io_controller ).
    IF lo_events IS NOT BOUND.
      RETURN.
    ENDIF.


    SYSTEM-CALL EVENTS GET NUM_HANDLERS
      FOR before_function OF INST lo_events INTO lv_evts.

    IF NOT lv_evts > 0.
      RETURN.
    ENDIF.

    handle_before_event( io_controller ).

    lo_events->raise_before_function(
      iv_function = iv_function
    ).

    handle_after_event( io_controller ).
  ENDMETHOD.


  METHOD raise_context_menu.
    DATA: lv_evts TYPE i.

    DATA(lo_events) = get_events( io_controller ).
    IF lo_events IS NOT BOUND.
      RETURN.
    ENDIF.


    SYSTEM-CALL EVENTS GET NUM_HANDLERS
      FOR context_menu OF INST lo_events INTO lv_evts.

    IF NOT lv_evts > 0.
      RETURN.
    ENDIF.

    handle_before_event( io_controller ).

    lo_events->raise_context_menu(
      io_menu
    ).

    handle_after_event( io_controller ).

  ENDMETHOD.


  METHOD raise_data_changed.
    DATA: lv_evts TYPE i.

    DATA(lo_events) = get_events( io_controller ).
    IF lo_events IS NOT BOUND.
      RETURN.
    ENDIF.


    SYSTEM-CALL EVENTS GET NUM_HANDLERS
      FOR data_changed OF INST lo_events INTO lv_evts.

    IF NOT lv_evts > 0.
      RETURN.
    ENDIF.

    handle_before_event( io_controller ).

    lo_events->raise_data_changed(
      ir_change_protocol = io_change_protocol
      if_onf4            = if_onf4
      if_onf4_before     = if_onf4_before
      if_onf4_after      = if_onf4_after
      iv_function        = iv_function
    ).

    " @TODO: check if causes an error, because refresh can normally not be fired during data_changed event of alv
    handle_after_event( io_controller ).
  ENDMETHOD.


  METHOD raise_double_click.
    DATA: lv_evts TYPE i.

    DATA(lo_events) = get_events( io_controller ).
    IF lo_events IS NOT BOUND.
      RETURN.
    ENDIF.


    SYSTEM-CALL EVENTS GET NUM_HANDLERS
      FOR double_click OF INST lo_events INTO lv_evts.

    IF NOT lv_evts > 0.
      RETURN.
    ENDIF.

    handle_before_event( io_controller ).

    lo_events->raise_double_click(
      iv_row    = iv_row
      iv_column = iv_column
    ).

    handle_after_event( io_controller ).
  ENDMETHOD.


  METHOD raise_drag.
    DATA: lv_evts TYPE i.

    DATA(lo_events) = get_events( io_controller ).
    IF lo_events IS NOT BOUND.
      RETURN.
    ENDIF.


    SYSTEM-CALL EVENTS GET NUM_HANDLERS
      FOR drag OF INST lo_events INTO lv_evts.

    IF NOT lv_evts > 0.
      RETURN.
    ENDIF.

    handle_before_event( io_controller ).

    lo_events->raise_drag(
        iv_row         = iv_row
        iv_column      = iv_column
        is_row_no      = is_row_no
        ir_dragdropobj = io_dragdropobj
    ).

    handle_after_event( io_controller ).
  ENDMETHOD.


  METHOD raise_drop.
    DATA: lv_evts TYPE i.

    DATA(lo_events) = get_events( io_controller ).
    IF lo_events IS NOT BOUND.
      RETURN.
    ENDIF.


    SYSTEM-CALL EVENTS GET NUM_HANDLERS
      FOR drop OF INST lo_events INTO lv_evts.

    IF NOT lv_evts > 0.
      RETURN.
    ENDIF.

    handle_before_event( io_controller ).

    lo_events->raise_drop(
        iv_row         = iv_row
        iv_column      = iv_column
        is_row_no      = is_row_no
        ir_dragdropobj = io_dragdropobj
    ).

    handle_after_event( io_controller ).
  ENDMETHOD.


  METHOD raise_drop_complete.
    DATA: lv_evts TYPE i.

    DATA(lo_events) = get_events( io_controller ).
    IF lo_events IS NOT BOUND.
      RETURN.
    ENDIF.


    SYSTEM-CALL EVENTS GET NUM_HANDLERS
      FOR drop_complete OF INST lo_events INTO lv_evts.

    IF NOT lv_evts > 0.
      RETURN.
    ENDIF.

    handle_before_event( io_controller ).

    lo_events->raise_drop_complete(
        iv_row         = iv_row
        iv_column      = iv_column
        is_row_no      = is_row_no
        ir_dragdropobj = io_dragdropobj
    ).

    handle_after_event( io_controller ).
  ENDMETHOD.


  METHOD raise_drop_get_flavor.
    DATA: lv_evts TYPE i.

    DATA(lo_events) = get_events( io_controller ).
    IF lo_events IS NOT BOUND.
      RETURN.
    ENDIF.


    SYSTEM-CALL EVENTS GET NUM_HANDLERS
      FOR drop_get_flavor OF INST lo_events INTO lv_evts.

    IF NOT lv_evts > 0.
      RETURN.
    ENDIF.

    handle_before_event( io_controller ).

    lo_events->raise_drop_get_flavor(
        iv_row         = iv_row
        iv_column      = iv_column
        is_row_no      = is_row_no
        ir_dragdropobj = io_dragdropobj
        it_flavors     = it_flavors
    ).

    handle_after_event( io_controller ).
  ENDMETHOD.


  METHOD raise_f4.
    DATA: lv_evts TYPE i.

    DATA(lo_events) = get_events( io_controller ).
    IF lo_events IS NOT BOUND.
      RETURN.
    ENDIF.


    SYSTEM-CALL EVENTS GET NUM_HANDLERS
      FOR f4 OF INST lo_events INTO lv_evts.

    IF NOT lv_evts > 0.
      RETURN.
    ENDIF.

    handle_before_event( io_controller ).

    lo_events->raise_f4(
        iv_fieldname  = iv_fieldname
        iv_fieldvalue = iv_fieldvalue
        is_row_no     = is_row_no
        ir_event_data = io_event_data
        it_bad_cells  = it_bad_cells
        if_display    = if_display
    ).

    " @TODO: check if causes an error because this call will refresh the alv
    handle_after_event( io_controller ).
  ENDMETHOD.


  METHOD raise_function_chosen.
    DATA: lv_evts TYPE i.

    DATA(lo_events) = get_events( io_controller ).
    IF lo_events IS NOT BOUND.
      RETURN.
    ENDIF.


    SYSTEM-CALL EVENTS GET NUM_HANDLERS
      FOR function_chosen OF INST lo_events INTO lv_evts.

    IF NOT lv_evts > 0.
      RETURN.
    ENDIF.

    handle_before_event( io_controller ).

    lo_events->raise_function_chosen(
      iv_function = iv_function
      iv_tag      = iv_tag
    ).

    handle_after_event( io_controller ).

  ENDMETHOD.


  METHOD raise_link_click.
    DATA: lv_evts TYPE i.

    DATA(lo_events) = get_events( io_controller ).
    IF lo_events IS NOT BOUND.
      RETURN.
    ENDIF.


    SYSTEM-CALL EVENTS GET NUM_HANDLERS
      FOR link_click OF INST lo_events INTO lv_evts.

    IF NOT lv_evts > 0.
      RETURN.
    ENDIF.

    handle_before_event( io_controller ).

    lo_events->raise_link_click(
      iv_row     = iv_row
      iv_column  = iv_column
    ).

    handle_after_event( io_controller ).
  ENDMETHOD.
ENDCLASS.
