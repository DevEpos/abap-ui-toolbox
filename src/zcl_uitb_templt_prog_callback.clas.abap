"! <p class="shorttext synchronized" lang="en">Callback for Template Program</p>
CLASS zcl_uitb_templt_prog_callback DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_uitb_view .
    INTERFACES zif_uitb_view_callback .
    INTERFACES zif_uitb_template_prog .

    TYPES:
      tt_controls TYPE STANDARD TABLE OF REF TO cl_gui_control WITH EMPTY KEY .
    TYPES:
      BEGIN OF ty_dynamic_function,
        function_id   TYPE fieldname,
        function_info TYPE smp_dyntxt,
      END OF ty_dynamic_function .
    TYPES:
      tt_dynamic_functions TYPE STANDARD TABLE OF ty_dynamic_function WITH DEFAULT KEY .

    "! <p class="shorttext synchronized" lang="en">Creates new template program</p>
    "!
    "! @parameter iv_title | <p class="shorttext synchronized" lang="en">Screen Title</p>
    "! @parameter rr_callback | <p class="shorttext synchronized" lang="en">Reference to the created template View</p>
    CLASS-METHODS create_template_program
      IMPORTING
        !iv_title          TYPE cua_tit_tx
      RETURNING
        VALUE(rr_callback) TYPE REF TO zif_uitb_template_prog .
    "! <p class="shorttext synchronized" lang="en">Frees allocated resources</p>
    METHODS free_resources .
    "! <p class="shorttext synchronized" lang="en">Raises the PBO event</p>
    METHODS raise_before_output
      IMPORTING
        !ir_callback TYPE REF TO zif_uitb_pbo_callback .
    "! <p class="shorttext synchronized" lang="en">Raises the exit event</p>
    METHODS raise_exit
      IMPORTING
        !ir_callback TYPE REF TO zif_uitb_exit_callback .
    "! <p class="shorttext synchronized" lang="en">Raises the PAI event</p>
    METHODS raise_user_command
      IMPORTING
        !iv_function TYPE sy-ucomm
        !ir_callback TYPE REF TO zif_uitb_pai_callback .
  PROTECTED SECTION.
    DATA: mv_program_title     TYPE cua_tit_tx,
          mt_dynamic_functions TYPE tt_dynamic_functions.

  PRIVATE SECTION.

    "! <p class="shorttext synchronized" lang="en">Custom container with special free logic for children</p>
    DATA mr_container TYPE REF TO cl_gui_custom_container .
    DATA mf_as_dialog TYPE abap_bool .
    DATA mt_registered_controls TYPE tt_controls .

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        !iv_title     TYPE cua_tit_tx.
ENDCLASS.



CLASS zcl_uitb_templt_prog_callback IMPLEMENTATION.


  METHOD constructor.
    mv_program_title = iv_title.
  ENDMETHOD.


  METHOD create_template_program.
    rr_callback = NEW zcl_uitb_templt_prog_callback(
        iv_title      = iv_title
    ).
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
    DATA(lv_function) = zif_uitb_template_prog=>c_func_leave.

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


  METHOD zif_uitb_view~is_visible.
  ENDMETHOD.


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
