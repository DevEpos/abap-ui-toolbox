class ZCL_UITB_TEMPLT_PROG_CALLBACK definition
  public
  final
  create private .

public section.

  interfaces ZIF_UITB_VIEW .
  interfaces ZIF_UITB_VIEW_CALLBACK .
  interfaces ZIF_UITB_TEMPLATE_PROG .

  types:
    tt_controls TYPE STANDARD TABLE OF REF TO cl_gui_control with EMPTY KEY .
  types:
    BEGIN OF ty_dynamic_function,
        function_id   TYPE fieldname,
        function_info TYPE smp_dyntxt,
      END OF ty_dynamic_function .
  types:
    tt_dynamic_functions TYPE STANDARD TABLE OF ty_dynamic_function WITH DEFAULT KEY .

  class-methods CREATE_TEMPLATE_PROGRAM
    importing
      !IV_TITLE type CUA_TIT_TX
    returning
      value(RR_CALLBACK) type ref to ZIF_UITB_TEMPLATE_PROG .
  methods FREE_RESOURCES .
  methods RAISE_BEFORE_OUTPUT
    importing
      !IR_CALLBACK type ref to ZIF_UITB_PBO_CALLBACK .
  methods RAISE_EXIT
    importing
      !IR_CALLBACK type ref to ZIF_UITB_EXIT_CALLBACK .
  methods RAISE_USER_COMMAND
    importing
      !IV_FUNCTION type SY-UCOMM
      !IR_CALLBACK type ref to ZIF_UITB_PAI_CALLBACK .
  PROTECTED SECTION.
    DATA: mv_program_title     TYPE cua_tit_tx,
          mt_dynamic_functions TYPE tt_dynamic_functions.

private section.

  data MR_CONTAINER type ref to CL_GUI_CUSTOM_CONTAINER .
  data MF_AS_DIALOG type ABAP_BOOL .
  data MT_REGISTERED_CONTROLS type TT_CONTROLS .

  methods CONSTRUCTOR
    importing
      !IV_TITLE type CUA_TIT_TX .
ENDCLASS.



CLASS ZCL_UITB_TEMPLT_PROG_CALLBACK IMPLEMENTATION.


  METHOD constructor.
    mv_program_title = iv_title.
  ENDMETHOD.


  METHOD create_template_program.
    rr_callback = NEW zcl_uitb_templt_prog_callback( iv_title ).
  ENDMETHOD.


  METHOD free_resources.
    IF mr_container IS BOUND.
      mr_container->free( ).
      cl_gui_cfw=>flush( ).
    ENDIF.

    CLEAR mr_container.

*.. Dispose of other registered resources
    LOOP AT mt_registered_controls INTO DATA(lr_control).
      IF lr_control IS BOUND.
        lr_control->free( ).
      ENDIF.
      DELETE mt_registered_controls.
    ENDLOOP.
  ENDMETHOD.


  METHOD raise_before_output.
    RAISE EVENT zif_uitb_view_callback~before_output
      EXPORTING
        er_callback = ir_callback.
  ENDMETHOD.


  METHOD raise_exit.
    RAISE EVENT zif_uitb_view_callback~exit
      EXPORTING
        er_callback = ir_callback.
  ENDMETHOD.


  METHOD raise_user_command.
    RAISE EVENT zif_uitb_view_callback~user_command
      EXPORTING
        ev_function_id = iv_function
        er_callback    = ir_callback.
  ENDMETHOD.


  METHOD zif_uitb_template_prog~add_control_to_lifecycle.
    mt_registered_controls = VALUE #(
      BASE mt_registered_controls ( ir_control )
    ).
  ENDMETHOD.


  METHOD zif_uitb_template_prog~add_function.
    APPEND VALUE ty_dynamic_function(
        function_id   = iv_function_id
        function_info = VALUE smp_dyntxt(
          text      = iv_text
          icon_id   = iv_icon
          icon_text = iv_icon_text
          quickinfo = COND #( WHEN iv_quickinfo IS NOT INITIAL THEN iv_quickinfo ELSE iv_text )
        )
    ) TO mt_dynamic_functions.
  ENDMETHOD.


  METHOD zif_uitb_template_prog~get_container.
    IF mr_container IS INITIAL.
      mr_container = NEW #(
          container_name = zif_uitb_template_prog~c_container_name
          lifetime       = cl_gui_container=>lifetime_dynpro
          repid          = zif_uitb_template_prog~c_template_prog_id
          dynnr          = COND #(
            WHEN mf_as_dialog = abap_true THEN
              zif_uitb_template_prog~c_template_prog_dialog_dynnr
            ELSE
              zif_uitb_template_prog~c_template_prog_dynnr
          )
      ).
    ENDIF.

    rr_container = mr_container.
  ENDMETHOD.


  METHOD zif_uitb_template_prog~leave_program.
    data(lv_function) = zif_uitb_template_prog=>c_func_leave.

    IF if_prevent_exit_event = abap_true.
      lv_function = zif_uitb_template_prog=>c_func_leave_no_exit_event.
    ENDIF.

    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
      EXPORTING
        functioncode = lv_function.
  ENDMETHOD.


  METHOD zif_uitb_template_prog~set_function_code.
    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
      EXPORTING
        functioncode = iv_function.
  ENDMETHOD.


  method ZIF_UITB_VIEW~IS_VISIBLE.
  endmethod.


  METHOD zif_uitb_view~show.
    mf_as_dialog = COND #( WHEN iv_start_column IS NOT INITIAL THEN abap_true ).

    CALL FUNCTION 'ZUITB_CALL_TEMPLATE_SCREEN'
      EXPORTING
        iv_program_title     = mv_program_title
        ir_callback          = me
        it_dynamic_functions = mt_dynamic_functions
        iv_start_line        = iv_start_line
        iv_end_line          = iv_end_line
        iv_start_column      = iv_start_column
        iv_end_column        = iv_end_column.

  ENDMETHOD.
ENDCLASS.
