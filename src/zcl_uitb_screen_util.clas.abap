"! <p class="shorttext synchronized" lang="en">Utilities for Screen Handling</p>
class ZCL_UITB_SCREEN_UTIL definition
  public
  final
  create public .

public section.

  "! <p class="shorttext synchronized" lang="en">Calls given screen</p>
  class-methods CALL_SCREEN
    importing
      !IV_SCREEN_ID type DYNNR
      !IV_REPORT_ID type SY-REPID
      !IF_SELSCREEN type ABAP_BOOL optional
      !IT_OBJECT_MAP type ZUITB_GLOBAL_OBJECT_MAP_T optional
      !IV_START_COLUMN type I optional
      !IV_START_LINE type I optional
      !IV_END_COLUMN type I optional
      !IV_END_LINE type I optional .
  "! <p class="shorttext synchronized" lang="en">Sets GUI Status for Selection Screen</p>
  class-methods SET_SELSCREEN_STATUS
    importing
      !IV_STATUS type SYST_PFKEY
      !IV_REPID type REPID optional
      !IT_EXCLUDING_FUNCTIONS type UI_FUNCTIONS optional .
  "! <p class="shorttext synchronized" lang="en">Leaves the current screen</p>
  class-methods LEAVE_SCREEN .
  "! <p class="shorttext synchronized" lang="en">Sets given function code to trigger PAI</p>
  class-methods SET_FUNCTION_CODE
    importing
      !IV_FUNCTION type UI_FUNC default '=' .
protected section.
private section.
ENDCLASS.



CLASS ZCL_UITB_SCREEN_UTIL IMPLEMENTATION.


  METHOD call_screen.
*... update controller reference
    DATA(lr_data_cache) = zcl_uitb_data_cache=>get_instance( iv_report_id ).

    LOOP AT it_object_map ASSIGNING FIELD-SYMBOL(<ls_global_object>).
      lr_data_cache->update_object_ref(
          iv_registered_name = |{ <ls_global_object>-variable_name }|
          ir_new_object_ref  = <ls_global_object>-global_ref
      ).
    ENDLOOP.

    DATA(lv_class) = |\\PROGRAM={ iv_report_id }\\CLASS=CL_SCREEN_UTIL|.

    CALL METHOD (lv_class)=>call_screen
      EXPORTING
        iv_screen_id        = iv_screen_id
        if_selection_screen = if_selscreen
        iv_start_line       = iv_start_line
        iv_start_column     = iv_start_column
        iv_end_line         = iv_end_line
        iv_end_column       = iv_end_column.

*... free resources
    LOOP AT it_object_map ASSIGNING <ls_global_object>.
      TRY .
          DATA(lr_screen_controller) = CAST zif_uitb_screen_controller( <ls_global_object>-global_ref ).
          lr_screen_controller->free_screen_resources( ).
        CATCH cx_sy_move_cast_error.
          CONTINUE.
      ENDTRY.
      lr_data_cache->clear_object_ref(
          iv_registered_name = |{ <ls_global_object>-variable_name }|
      ).
    ENDLOOP.
  ENDMETHOD.


  METHOD leave_screen.
    SET SCREEN 0.
    LEAVE SCREEN.
  ENDMETHOD.


  METHOD set_function_code.
    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
      EXPORTING
        functioncode = iv_function.
  ENDMETHOD.


  METHOD set_selscreen_status.
    DATA: lt_exclude TYPE ui_functions.

    lt_exclude = it_excluding_functions.

    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING
        p_status  = iv_status
        p_program = iv_repid
      TABLES
        p_exclude = lt_exclude.
  ENDMETHOD.
ENDCLASS.
