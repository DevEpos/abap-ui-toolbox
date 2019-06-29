CLASS zcl_uitb_alv_events DEFINITION
  PUBLIC
  INHERITING FROM zcl_uitb_alv_metadata
  FINAL
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_uitb_alv_evt_controller .

  PUBLIC SECTION.

    EVENTS data_changed
      EXPORTING
        VALUE(er_change_protocol) TYPE REF TO cl_alv_changed_data_protocol OPTIONAL
        VALUE(ef_onf4) TYPE abap_bool OPTIONAL
        VALUE(ef_onf4_before) TYPE abap_bool OPTIONAL
        VALUE(ef_onf4_after) TYPE abap_bool OPTIONAL
        VALUE(ev_function) TYPE sy-ucomm OPTIONAL .
    EVENTS f4
      EXPORTING
        VALUE(ev_fieldname) TYPE lvc_fname OPTIONAL
        VALUE(ev_fieldvalue) TYPE lvc_value OPTIONAL
        VALUE(es_row_no) TYPE lvc_s_roid OPTIONAL
        VALUE(er_event_data) TYPE REF TO cl_alv_event_data OPTIONAL
        VALUE(et_bad_cells) TYPE lvc_t_modi OPTIONAL
        VALUE(ef_display) TYPE abap_bool OPTIONAL .
    EVENTS before_function
      EXPORTING
        VALUE(ev_function) TYPE ui_func OPTIONAL .
    EVENTS after_function
      EXPORTING
        VALUE(ev_function) TYPE ui_func OPTIONAL .
    "! Event for chosen ALV function e.g. toolbar button clicked
    "!
    "! @parameter ev_function | the function code
    "! @parameter ev_tag      | the tag of the function
    EVENTS function_chosen
      EXPORTING
        VALUE(ev_function) TYPE ui_func OPTIONAL
        VALUE(ev_tag) TYPE string OPTIONAL .
    EVENTS double_click
      EXPORTING
        VALUE(ev_row) TYPE lvc_roid
        VALUE(ev_column) TYPE lvc_fname .
    EVENTS link_click
      EXPORTING
        VALUE(ev_row) TYPE i
        VALUE(ev_column) TYPE lvc_fname .
    EVENTS drop_get_flavor
      EXPORTING
        VALUE(ev_row) TYPE i
        VALUE(ev_column) TYPE lvc_fname
        VALUE(es_row_no) TYPE lvc_s_roid
        VALUE(er_dragdropobj) TYPE REF TO cl_dragdropobject
        VALUE(et_flavors) TYPE cndd_flavors .
    EVENTS drag
      EXPORTING
        VALUE(ev_row) TYPE i
        VALUE(ev_column) TYPE lvc_fname
        VALUE(es_row_no) TYPE lvc_s_roid
        VALUE(er_dragdropobj) TYPE REF TO cl_dragdropobject .
    EVENTS drop
      EXPORTING
        VALUE(ev_row) TYPE i
        VALUE(ev_column) TYPE lvc_fname
        VALUE(es_row_no) TYPE lvc_s_roid
        VALUE(er_dragdropobj) TYPE REF TO cl_dragdropobject .
    EVENTS drop_complete
      EXPORTING
        VALUE(ev_row) TYPE i
        VALUE(ev_column) TYPE lvc_fname
        VALUE(es_row_no) TYPE lvc_s_roid
        VALUE(er_dragdropobj) TYPE REF TO cl_dragdropobject .
    EVENTS context_menu
      EXPORTING
        VALUE(er_menu) TYPE REF TO cl_ctmenu .
    METHODS get_sender
      RETURNING
        VALUE(ro_alv) TYPE REF TO zcl_uitb_alv.
  PROTECTED SECTION.

    METHODS raise_drop_get_flavor
      IMPORTING
        !iv_row         TYPE i
        !iv_column      TYPE lvc_fname
        !is_row_no      TYPE lvc_s_roid
        !ir_dragdropobj TYPE REF TO cl_dragdropobject
        !it_flavors     TYPE cndd_flavors OPTIONAL .
    METHODS raise_drag
      IMPORTING
        !iv_row         TYPE i
        !iv_column      TYPE lvc_fname
        !is_row_no      TYPE lvc_s_roid
        !ir_dragdropobj TYPE REF TO cl_dragdropobject OPTIONAL .
    METHODS raise_drop
      IMPORTING
        !iv_row         TYPE i
        !iv_column      TYPE lvc_fname
        !is_row_no      TYPE lvc_s_roid
        !ir_dragdropobj TYPE REF TO cl_dragdropobject OPTIONAL .
    METHODS raise_drop_complete
      IMPORTING
        !iv_row         TYPE i
        !iv_column      TYPE lvc_fname
        !is_row_no      TYPE lvc_s_roid
        !ir_dragdropobj TYPE REF TO cl_dragdropobject OPTIONAL .
    METHODS raise_function_chosen
      IMPORTING
        !iv_function TYPE ui_func
        !iv_tag      TYPE string OPTIONAL .
    METHODS raise_after_function
      IMPORTING
        !iv_function TYPE ui_func .
    METHODS raise_before_function
      IMPORTING
        !iv_function TYPE ui_func .
    METHODS raise_link_click
      IMPORTING
        !iv_row    TYPE i
        !iv_column TYPE lvc_fname .
    METHODS raise_double_click
      IMPORTING
        !iv_row    TYPE i
        !iv_column TYPE lvc_fname .
    METHODS raise_f4
      IMPORTING
        !iv_fieldname  TYPE lvc_fname
        !iv_fieldvalue TYPE lvc_value
        !is_row_no     TYPE lvc_s_roid
        !ir_event_data TYPE REF TO cl_alv_event_data
        !it_bad_cells  TYPE lvc_t_modi OPTIONAL
        !if_display    TYPE abap_bool OPTIONAL .
    METHODS raise_data_changed
      IMPORTING
        !ir_change_protocol TYPE REF TO cl_alv_changed_data_protocol OPTIONAL
        !if_onf4            TYPE abap_bool OPTIONAL
        !if_onf4_before     TYPE abap_bool OPTIONAL
        !if_onf4_after      TYPE abap_bool OPTIONAL
        !iv_function        TYPE sy-ucomm OPTIONAL .
    METHODS raise_context_menu
      IMPORTING
        !ir_menu TYPE REF TO cl_ctmenu .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_uitb_alv_events IMPLEMENTATION.

  METHOD get_sender.
    ro_alv = CAST zcl_uitb_alv_controller( mr_controller )->mr_model.
  ENDMETHOD.

  METHOD raise_after_function.
    RAISE EVENT after_function
      EXPORTING
        ev_function = iv_function.
  ENDMETHOD.


  METHOD raise_before_function.
    RAISE EVENT before_function
      EXPORTING
        ev_function = iv_function.
  ENDMETHOD.


  METHOD raise_context_menu.
    RAISE EVENT context_menu
      EXPORTING
        er_menu = ir_menu.
  ENDMETHOD.


  METHOD raise_data_changed.
    RAISE EVENT data_changed
      EXPORTING
        er_change_protocol = ir_change_protocol
        ef_onf4            = if_onf4
        ef_onf4_before     = if_onf4_before
        ef_onf4_after      = if_onf4_after
        ev_function        = iv_function.
  ENDMETHOD.


  METHOD raise_double_click.
    RAISE EVENT double_click
      EXPORTING
        ev_row    = iv_row
        ev_column = iv_column.
  ENDMETHOD.


  METHOD raise_drag.
    RAISE EVENT drag
      EXPORTING
        ev_row         = iv_row
        ev_column      = iv_column
        es_row_no      = is_row_no
        er_dragdropobj = ir_dragdropobj.
  ENDMETHOD.


  METHOD raise_drop.
    RAISE EVENT drop
      EXPORTING
        ev_row         = iv_row
        ev_column      = iv_column
        es_row_no      = is_row_no
        er_dragdropobj = ir_dragdropobj.
  ENDMETHOD.


  METHOD raise_drop_complete.
    RAISE EVENT drop_complete
      EXPORTING
        ev_row         = iv_row
        ev_column      = iv_column
        es_row_no      = is_row_no
        er_dragdropobj = ir_dragdropobj.
  ENDMETHOD.


  METHOD raise_drop_get_flavor.
    RAISE EVENT drop_get_flavor
      EXPORTING
        ev_row         = iv_row
        ev_column      = iv_column
        es_row_no      = is_row_no
        er_dragdropobj = ir_dragdropobj
        et_flavors     = it_flavors.
  ENDMETHOD.


  METHOD raise_f4.
    RAISE EVENT f4
      EXPORTING
        ev_fieldname  = iv_fieldname
        ev_fieldvalue = iv_fieldvalue
        es_row_no     = is_row_no
        er_event_data = ir_event_data
        et_bad_cells  = it_bad_cells
        ef_display    = if_display.
  ENDMETHOD.


  METHOD raise_function_chosen.
    RAISE EVENT function_chosen
      EXPORTING
        ev_function = iv_function
        ev_tag      = iv_tag.
  ENDMETHOD.


  METHOD raise_link_click.
    RAISE EVENT link_click
      EXPORTING
        ev_row    = iv_row
        ev_column = iv_column.
  ENDMETHOD.

ENDCLASS.
