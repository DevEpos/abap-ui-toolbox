"! <p class="shorttext synchronized" lang="en">Utilities for Screen Handling</p>
CLASS zcl_uitb_screen_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: ty_metric TYPE c LENGTH 1.
    TYPES:
      BEGIN OF ty_s_size,
        metric TYPE ty_metric,
        width  TYPE i,
        height TYPE i,
      END OF ty_s_size.
    TYPES:
      BEGIN OF ty_s_area ,
        metric TYPE ty_metric,
        left   TYPE i,
        top    TYPE i,
        right  TYPE i,
        bottom TYPE i,
      END   OF ty_s_area .
    CONSTANTS:
      BEGIN  OF c_metrics,
        dynpro     TYPE ty_metric VALUE '1',
        pixel      TYPE ty_metric VALUE '2',
        millimeter TYPE ty_metric VALUE '3',
        relative   TYPE ty_metric VALUE '4',
      END OF c_metrics.

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
    "!
    CLASS-METHODS set_function_code
      IMPORTING
        !iv_function TYPE ui_func DEFAULT '=' .

    "! <p class="shorttext synchronized" lang="en">Remove Screen toolbar</p>
    "!
    CLASS-METHODS remove_screen_toolbar
      IMPORTING
        iv_program   TYPE sy-repid
        iv_screen_id TYPE sy-dynnr.

    "! <p class="shorttext synchronized" lang="en">Retrieves the area for the given size</p>
    "!
    CLASS-METHODS get_screen_area_for_size
      IMPORTING
        is_size        TYPE ty_s_size
        iv_metric      TYPE ty_metric
      RETURNING
        VALUE(rs_area) TYPE ty_s_area.
    "! <p class="shorttext synchronized" lang="en">Convert to the given size to the given metric</p>
    "!
    CLASS-METHODS convert_size_metric
      IMPORTING
        !is_size       TYPE ty_s_size
        !iv_metric     TYPE ty_metric
      RETURNING
        VALUE(rs_size) TYPE ty_s_size.
    "! <p class="shorttext synchronized" lang="en">Set new current gui command object</p>
    "!
    CLASS-METHODS set_current_command
      IMPORTING
        io_command TYPE REF TO zif_uitb_gui_command_executor OPTIONAL.
    "! <p class="shorttext synchronized" lang="en">Get current gui command object</p>
    "!
    CLASS-METHODS get_current_command
      RETURNING
        VALUE(ro_command) TYPE REF TO zif_uitb_gui_command_executor.
    "! <p class="shorttext synchronized" lang="en">Executes possible GUI command</p>
    "!
    CLASS-METHODS handle_gui_command
      CHANGING
        cv_ok_code TYPE sy-ucomm.
    "! <p class="shorttext synchronized" lang="en">Trigger new PAI to handle GUI command</p>
    "!
    CLASS-METHODS raise_gui_command.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA go_current_command TYPE REF TO zif_uitb_gui_command_executor.
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

    DATA(lv_class) = |\\PROGRAM={ iv_report_id }\\CLASS=LCL_SCREEN_UTIL|.

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
      IMPORTING
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

  METHOD convert_size_metric.
    DATA: ls_factors TYPE cntl_metric_factors.

    IF iv_metric IS INITIAL.
      rs_size = is_size.
    ELSE.
      CASE is_size-metric.

        WHEN space.
          CLEAR rs_size.

        WHEN iv_metric.
          rs_size = is_size.

        WHEN c_metrics-pixel.
          CASE iv_metric.
            WHEN c_metrics-dynpro.
              rs_size-metric = c_metrics-dynpro.
              rs_size-width  = cl_gui_cfw=>compute_dynp_from_pixels( x_or_y = 'X' in = is_size-width  ).
              rs_size-height = cl_gui_cfw=>compute_dynp_from_pixels( x_or_y = 'Y' in = is_size-height ).
            WHEN c_metrics-millimeter.
              rs_size-metric = c_metrics-millimeter.
              rs_size-width  = cl_gui_cfw=>compute_dynp_from_pixels( x_or_y = 'X' in = is_size-width  ).
              rs_size-height = cl_gui_cfw=>compute_dynp_from_pixels( x_or_y = 'Y' in = is_size-height ).
              rs_size-width  = cl_gui_cfw=>compute_metric_from_dynp( metric = cl_gui_control=>metric_mm x_or_y = 'X' in = rs_size-width  ).
              rs_size-height = cl_gui_cfw=>compute_metric_from_dynp( metric = cl_gui_control=>metric_mm x_or_y = 'Y' in = rs_size-height ).
            WHEN OTHERS.
              CLEAR rs_size.
          ENDCASE.

        WHEN c_metrics-dynpro.
          CASE iv_metric.
            WHEN c_metrics-pixel.
              rs_size-metric = c_metrics-pixel.
              rs_size-width  = cl_gui_cfw=>compute_metric_from_dynp( metric = cl_gui_control=>metric_pixel x_or_y = 'X' in = is_size-width  ).
              rs_size-height = cl_gui_cfw=>compute_metric_from_dynp( metric = cl_gui_control=>metric_pixel x_or_y = 'Y' in = is_size-height ).
            WHEN c_metrics-millimeter.
              rs_size-metric = c_metrics-millimeter.
              rs_size-width  = cl_gui_cfw=>compute_metric_from_dynp( metric = cl_gui_control=>metric_mm x_or_y = 'X' in = is_size-width  ).
              rs_size-height = cl_gui_cfw=>compute_metric_from_dynp( metric = cl_gui_control=>metric_mm x_or_y = 'Y' in = is_size-height ).
            WHEN OTHERS.
              CLEAR rs_size.
          ENDCASE.

        WHEN c_metrics-millimeter.
          CASE iv_metric.
            WHEN c_metrics-dynpro.
              rs_size-metric = c_metrics-dynpro.
              rs_size-width  = cl_gui_cfw=>compute_pixel_from_metric( metric = cl_gui_control=>metric_mm x_or_y = 'X' in = is_size-width  ).
              rs_size-height = cl_gui_cfw=>compute_pixel_from_metric( metric = cl_gui_control=>metric_mm x_or_y = 'Y' in = is_size-height ).
              rs_size-width  = cl_gui_cfw=>compute_dynp_from_pixels( x_or_y = 'X' in = rs_size-width  ).
              rs_size-height = cl_gui_cfw=>compute_dynp_from_pixels( x_or_y = 'Y' in = rs_size-height ).
            WHEN c_metrics-pixel.
              rs_size-metric = c_metrics-pixel.
              rs_size-width  = cl_gui_cfw=>compute_pixel_from_metric( metric = cl_gui_control=>metric_mm x_or_y = 'X' in = is_size-width  ).
              rs_size-height = cl_gui_cfw=>compute_pixel_from_metric( metric = cl_gui_control=>metric_mm x_or_y = 'Y' in = is_size-height ).
            WHEN OTHERS.
              CLEAR rs_size.
          ENDCASE.

        WHEN c_metrics-relative.
          rs_size-metric = c_metrics-relative.
          ls_factors = cl_gui_cfw=>get_metric_factors( ).
          IF ls_factors-screen-x IS INITIAL.
            ls_factors-screen-x = 1280.
          ENDIF.
          IF ls_factors-screen-y IS INITIAL.
            ls_factors-screen-y = 1024.
          ENDIF.
          IF is_size-width > 100.
            rs_size-width  = ls_factors-screen-x.
          ELSE.
            rs_size-width  = ls_factors-screen-x * is_size-width  / 100.
          ENDIF.
          IF is_size-height > 100.
            rs_size-height = ls_factors-screen-y.
          ELSE.
            rs_size-height = ls_factors-screen-y * is_size-height / 100.
          ENDIF.
          CASE iv_metric.
            WHEN c_metrics-pixel.
              RETURN.
            WHEN c_metrics-dynpro.
              rs_size-metric = c_metrics-dynpro.
              rs_size-width  = cl_gui_cfw=>compute_dynp_from_pixels( x_or_y = 'X' in = rs_size-width  ).
              rs_size-height = cl_gui_cfw=>compute_dynp_from_pixels( x_or_y = 'Y' in = rs_size-height ).
            WHEN c_metrics-millimeter.
              rs_size-metric = c_metrics-millimeter.
              rs_size-width  = cl_gui_cfw=>compute_dynp_from_pixels( x_or_y = 'X' in = rs_size-width  ).
              rs_size-height = cl_gui_cfw=>compute_dynp_from_pixels( x_or_y = 'Y' in = rs_size-height ).
              rs_size-width  = cl_gui_cfw=>compute_metric_from_dynp( metric = cl_gui_control=>metric_mm x_or_y = 'X' in = rs_size-width  ).
              rs_size-height = cl_gui_cfw=>compute_metric_from_dynp( metric = cl_gui_control=>metric_mm x_or_y = 'Y' in = rs_size-height ).
            WHEN OTHERS.
              CLEAR rs_size.
          ENDCASE.
        WHEN OTHERS.
          CLEAR rs_size.
      ENDCASE.
    ENDIF.

  ENDMETHOD.

  METHOD get_screen_area_for_size.
    DATA(ls_window_metrics) = cl_gui_cfw=>get_metric_factors( ).

    DATA(ls_window_size) = COND #( WHEN iv_metric = c_metrics-dynpro THEN
                                      VALUE ty_s_size( width = 254 height = 200 )
                                   ELSE
                                        convert_size_metric( is_size   = VALUE #(
                                                                metric = c_metrics-pixel
                                                                width = ls_window_metrics-screen-x
                                                                height = ls_window_metrics-screen-y )
                                                             iv_metric = iv_metric )
    ).
    DATA(ls_screen_size) = convert_size_metric(
       is_size   = is_size
       iv_metric = iv_metric
    ).

    DATA(lv_top) = ( ls_window_size-height - ls_screen_size-height ) / 2.
    DATA(lv_left) = ( ls_window_size-width  - ls_screen_size-width ) / 2.
    rs_area = VALUE #(
      top    = lv_top
      left   = lv_left
      right  = lv_left + ls_screen_size-width
      bottom = lv_top + ls_screen_size-height
    ).
  ENDMETHOD.

  METHOD get_current_command.
    ro_command = go_current_command.
  ENDMETHOD.

  METHOD set_current_command.
    go_current_command = io_command.
  ENDMETHOD.

  METHOD raise_gui_command.
    cl_gui_cfw=>set_new_ok_code( EXPORTING new_code = zif_uitb_gui_command_executor=>c_ucomm_prefix
                                 IMPORTING rc       = DATA(lv_subrc) ).
*.. Remove command if rc <> 0 ???
  ENDMETHOD.

  METHOD handle_gui_command.
    IF go_current_command IS BOUND.
      CLEAR cv_ok_code.
      DATA(lo_command) = go_current_command.
      CLEAR go_current_command.
      lo_command->execute( ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
