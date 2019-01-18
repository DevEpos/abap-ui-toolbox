"! <p class="shorttext synchronized" lang="en">Utilities for Screen Handling</p>
CLASS zcl_uitb_screen_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">Calls given screen</p>
    CLASS-METHODS call_screen
      IMPORTING
        !iv_screen_id    TYPE dynnr
        !iv_report_id    TYPE sy-repid
        !if_selscreen    TYPE abap_bool OPTIONAL
        !it_object_map   TYPE zuitb_global_object_map_t OPTIONAL
        !iv_start_column TYPE i OPTIONAL
        !iv_start_line   TYPE i OPTIONAL
        !iv_end_column   TYPE i OPTIONAL
        !iv_end_line     TYPE i OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Sets GUI Status for Selection Screen</p>
    CLASS-METHODS set_selscreen_status
      IMPORTING
        !iv_status              TYPE syst_pfkey
        !iv_repid               TYPE repid OPTIONAL
        !it_excluding_functions TYPE ui_functions OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Leaves the current screen</p>
    CLASS-METHODS leave_screen .
    "! <p class="shorttext synchronized" lang="en">Show Progress indicator with text</p>
    "!
    CLASS-METHODS show_progress
      IMPORTING
        iv_progress TYPE i DEFAULT 0
        !iv_text    TYPE string .
    "! <p class="shorttext synchronized" lang="en">Sets given function code to trigger PAI</p>
    CLASS-METHODS set_function_code
      IMPORTING
        !iv_function TYPE ui_func DEFAULT '=' .

    "! <p class="shorttext synchronized" lang="en">Remove Screen toolbar</p>
    CLASS-METHODS remove_screen_toolbar
      IMPORTING
        iv_program   TYPE sy-repid
        iv_screen_id TYPE sy-dynnr.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_uitb_screen_util IMPLEMENTATION.


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
    lr_data_cache = zcl_uitb_data_cache=>get_instance( iv_report_id ).
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

  METHOD show_progress.
    DATA(lv_progress) = COND #( WHEN iv_progress <= 0 THEN 0
                                WHEN iv_progress > 100 THEN 100
                                ELSE iv_progress ).

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = lv_progress
        text       = iv_text.
  ENDMETHOD.

  METHOD remove_screen_toolbar.
    DATA: ls_header               TYPE rpy_dyhead,
          lt_containers           TYPE dycatt_tab,
          lt_fields_to_containers TYPE dyfatc_tab,
          lt_flow_logic           TYPE swydyflow.

    CALL FUNCTION 'RPY_DYNPRO_READ'
      EXPORTING
        progname             = iv_program
        dynnr                = iv_screen_id
      importing
        header               = ls_header
      TABLES
        containers           = lt_containers
        fields_to_containers = lt_fields_to_containers
        flow_logic           = lt_flow_logic
      EXCEPTIONS
        cancelled            = 1
        not_found            = 2
        permission_error     = 3
        OTHERS               = 4.
    IF sy-subrc IS NOT INITIAL.
      RETURN. " Ignore errors, just exit
    ENDIF.

    IF ls_header-no_toolbar = abap_true.
      RETURN. " No change required
    ENDIF.

    ls_header-no_toolbar = abap_true.

    CALL FUNCTION 'RPY_DYNPRO_INSERT'
      EXPORTING
        header                 = ls_header
        suppress_exist_checks  = abap_true
      TABLES
        containers             = lt_containers
        fields_to_containers   = lt_fields_to_containers
        flow_logic             = lt_flow_logic
      EXCEPTIONS
        cancelled              = 1
        already_exists         = 2
        program_not_exists     = 3
        not_executed           = 4
        missing_required_field = 5
        illegal_field_value    = 6
        field_not_allowed      = 7
        not_generated          = 8
        illegal_field_position = 9
        OTHERS                 = 10.
    IF sy-subrc <> 2 AND sy-subrc <> 0.
      RETURN. " Ignore errors, just exit
    ENDIF.
  ENDMETHOD.

ENDCLASS.
