CLASS zcl_uitb_alv_data DEFINITION
  PUBLIC
  INHERITING FROM zcl_uitb_alv_metadata
  FINAL
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_uitb_alv_grid_adapter .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !ir_controller TYPE REF TO zif_uitb_alv_metadata_ctrller
        !ir_data       TYPE REF TO data .
    METHODS check_changed
      RETURNING
        VALUE(rf_valid) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Sets container for application log</p>
    "!
    "! @parameter io_container | <p class="shorttext synchronized" lang="en">Container reference for log</p>
    METHODS set_container_for_log
      IMPORTING
        io_container TYPE REF TO cl_gui_container.
    METHODS set_change_event
      IMPORTING
        !iv_event_id TYPE i DEFAULT cl_gui_alv_grid=>mc_evt_enter .
  PROTECTED SECTION.
  PRIVATE SECTION.
    data mo_applog_container type ref to cl_gui_container.
    DATA mr_data TYPE REF TO data .
    DATA mv_edit_event TYPE i VALUE cl_gui_alv_grid=>mc_evt_enter ##NO_TEXT.
ENDCLASS.



CLASS zcl_uitb_alv_data IMPLEMENTATION.


  METHOD check_changed.
    DATA(lr_model) = CAST zcl_uitb_alv_controller( mr_controller ).
    DATA(lr_adapter) = lr_model->mo_adapter.

    DATA(lr_grid) = lr_adapter->get_grid( ).

    lr_grid->check_changed_data(
      IMPORTING
          e_valid   = rf_valid    " Eingaben sind konsistent
*      CHANGING
*        c_refresh = 'X'    " Charakterfeld der Länge 1
    ).

  ENDMETHOD.


  METHOD constructor.
    super->constructor( io_controller = ir_controller ).

    mr_data = ir_data.
  ENDMETHOD.


  METHOD set_change_event.
    CHECK iv_event_id = cl_gui_alv_grid=>mc_evt_enter OR iv_event_id = cl_gui_alv_grid=>mc_evt_modified.

    mv_edit_event = iv_event_id.
  ENDMETHOD.

  METHOD set_container_for_log.
    mo_applog_container = io_container.
  ENDMETHOD.

ENDCLASS.
