class ZCL_UITB_ALV_DATA definition
  public
  inheriting from ZCL_UITB_ALV_METADATA
  final
  create public

  global friends ZCL_UITB_ALV_GRID_ADAPTER .

public section.

  methods CONSTRUCTOR
    importing
      !IR_CONTROLLER type ref to ZIF_UITB_ALV_METADATA_CTRLLER
      !IR_DATA type ref to DATA .
  methods CHECK_CHANGED
    returning
      value(RF_VALID) type ABAP_BOOL .
  methods SET_CHANGE_EVENT
    importing
      !IV_EVENT_ID type I default CL_GUI_ALV_GRID=>MC_EVT_ENTER .
  PROTECTED SECTION.
private section.

  data MR_DATA type ref to DATA .
  data MV_EDIT_EVENT type I value CL_GUI_ALV_GRID=>MC_EVT_ENTER ##NO_TEXT.
ENDCLASS.



CLASS ZCL_UITB_ALV_DATA IMPLEMENTATION.


  METHOD check_changed.
    data(lr_model) = cast ZCL_UITB_alv_controller( mr_controller ).
    data(lr_adapter) = lr_model->mr_adapter.

    DATA(lr_grid) = lr_adapter->get_grid( ).

    lr_grid->check_changed_data(
      IMPORTING
          e_valid   = rf_valid    " Eingaben sind konsistent
*      CHANGING
*        c_refresh = 'X'    " Charakterfeld der Länge 1
    ).

  ENDMETHOD.


  METHOD constructor.
    super->constructor( ir_controller = ir_controller ).

    mr_data = ir_data.
  ENDMETHOD.


  METHOD set_change_event.
    CHECK iv_event_id = cl_gui_alv_grid=>mc_evt_enter OR iv_event_id = cl_gui_alv_grid=>mc_evt_modified.

    mv_edit_event = iv_event_id.
  ENDMETHOD.
ENDCLASS.
