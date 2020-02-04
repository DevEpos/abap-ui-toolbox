"! <p class="shorttext synchronized" lang="en">Generic GUI Screen</p>
CLASS zcl_uitb_gui_screen_base DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_uitb_gui_screen.
    INTERFACES zif_uitb_gui_command_handler
      ALL METHODS ABSTRACT.

    ALIASES mf_visible
      FOR zif_uitb_gui_screen~mf_visible.
    ALIASES show
      FOR zif_uitb_gui_screen~show.
    ALIASES leave_screen
      FOR zif_uitb_gui_screen~leave_screen.
    ALIASES execute_command
      FOR zif_uitb_gui_command_handler~execute_command.

    TYPES tt_controls TYPE STANDARD TABLE OF REF TO cl_gui_control WITH EMPTY KEY .

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    "! Creates new generic screen
    "!
    METHODS constructor
      IMPORTING
        iv_title    TYPE string
        iv_lifetime TYPE i DEFAULT cl_gui_control=>lifetime_dynpro.
  PROTECTED SECTION.
    DATA mv_title TYPE string.
    DATA mf_screen_is_live TYPE abap_bool.
    DATA mf_as_dialog TYPE abap_bool.

    DATA mo_container TYPE REF TO cl_gui_container.

    DATA mv_lifetime TYPE i VALUE cl_gui_control=>lifetime_dynpro.

    "! <p class="shorttext synchronized" lang="en">Creates the main container of the screen</p>
    "!
    METHODS create_container.
    "! <p class="shorttext synchronized" lang="en">Create content of screen</p>
    "!
    METHODS create_content
          ABSTRACT
      IMPORTING
        io_container TYPE REF TO cl_gui_container
      RAISING
        zcx_uitb_gui_exception.
    "! <p class="shorttext synchronized" lang="en">Handle UI before output</p>
    "!
    METHODS do_before_dynpro_output
      IMPORTING
        io_callback TYPE REF TO zif_uitb_gui_pbo_callback.
    "! <p class="shorttext synchronized" lang="en">Handles exit request of dynpro</p>
    "!
    METHODS handle_exit_request
      IMPORTING
        io_callback TYPE REF TO zif_uitb_exit_callback.
    "! <p class="shorttext synchronized" lang="en">Register handlers for the given toolbar</p>
    "!
    METHODS register_toolbar
          FINAL
      IMPORTING
        io_toolbar TYPE REF TO cl_gui_toolbar.
    "! <p class="shorttext synchronized" lang="en">Creates control toolbar</p>
    "!
    METHODS create_control_toolbar
          FINAL
      IMPORTING
        io_parent    TYPE REF TO cl_gui_container
        iv_mode      TYPE i DEFAULT cl_gui_toolbar=>m_mode_horizontal
        if_show_sash TYPE abap_bool OPTIONAL
        !it_button   TYPE ttb_button OPTIONAL
      EXPORTING
        !eo_toolbar  TYPE REF TO cl_gui_toolbar
        !eo_client   TYPE REF TO cl_gui_container
      RAISING
        zcx_uitb_gui_exception .
    "! <p class="shorttext synchronized" lang="en">Raise the given function as command</p>
    "!
    METHODS trigger_command
          FINAL
      IMPORTING
        iv_function TYPE ui_func.

    "! <p class="shorttext synchronized" lang="en">Final call after Screen was left</p>
    "!
    METHODS do_after_exit.
  PRIVATE SECTION.
    "! <p class="shorttext synchronized" lang="en">Handler for PBO of Dynpro</p>
    "!
    METHODS on_dynpro_before_output
        FOR EVENT before_output OF zif_uitb_gui_dynpro_events
      IMPORTING
        eo_callback.
    "! <p class="shorttext synchronized" lang="en">Handler for PAI of Dynpro</p>
    "!
    METHODS on_dynpro_user_command
        FOR EVENT user_command OF zif_uitb_gui_dynpro_events
      IMPORTING
        ev_function_id.
    "! <p class="shorttext synchronized" lang="en">Handler for Exit code of dynpro</p>
    "!
    METHODS on_exit
        FOR EVENT exit OF zif_uitb_gui_dynpro_events
      IMPORTING
        eo_callback.
    "! <p class="shorttext synchronized" lang="en">Handler for dynpro was exited</p>
    "!
    METHODS on_exited
        FOR EVENT exited OF zif_uitb_gui_dynpro_events.

    "! <p class="shorttext synchronized" lang="en">Handler for function click of toolbar button</p>
    "!
    METHODS on_toolbar_function
        FOR EVENT function_selected OF cl_gui_toolbar
      IMPORTING
        fcode.
    "! <p class="shorttext synchronized" lang="en">Handler for toolbar dropdown click of toolbar</p>
    "!
    METHODS on_toolbar_dropdown
        FOR EVENT dropdown_clicked OF cl_gui_toolbar
      IMPORTING
        fcode
        posx
        posy
        sender.
ENDCLASS.



CLASS zcl_uitb_gui_screen_base IMPLEMENTATION.
  METHOD constructor.
    mv_title = iv_title.
    mv_lifetime = iv_lifetime.
  ENDMETHOD.


  METHOD zif_uitb_gui_screen~leave_screen.
    DATA(lv_function) = zif_uitb_c_gui_screen=>c_functions-leave.

    IF if_prevent_exit_event = abap_true.
      lv_function = zif_uitb_c_gui_screen=>c_functions-leave_no_exit_event.
    ENDIF.

    zcl_uitb_screen_util=>set_function_code( lv_function ).
  ENDMETHOD.

  METHOD zif_uitb_gui_screen~show.
    DATA(lo_dynpro_handler) = NEW zcl_uitb_gui_dynpro_handler( ).
    SET HANDLER:
      on_dynpro_before_output FOR lo_dynpro_handler,
      on_dynpro_user_command  FOR lo_dynpro_handler,
      on_exit                 FOR lo_dynpro_handler,
      on_exited               FOR lo_dynpro_handler.

    IF mf_as_dialog = abap_true.
      DATA(lv_start_line) = COND #( WHEN iv_top IS INITIAL THEN 2 ELSE iv_top ).
      DATA(lv_start_column) = COND #( WHEN iv_left IS INITIAL THEN 10 ELSE iv_left ).

      CALL FUNCTION 'ZUITB_CALL_GUI_SCREEN'
        EXPORTING
          iv_program_title = CONV cua_tit_tx( mv_title )
          io_callback      = lo_dynpro_handler
          iv_start_line    = lv_start_line
          iv_end_line      = lv_start_line + iv_height
          iv_start_column  = lv_start_column
          iv_end_column    = lv_start_column + iv_width.
    ELSE.
      CALL FUNCTION 'ZUITB_CALL_GUI_SCREEN'
        EXPORTING
          iv_program_title = CONV cua_tit_tx( mv_title )
          io_callback      = lo_dynpro_handler.
    ENDIF.
  ENDMETHOD.


  METHOD create_container.
    mo_container = NEW cl_gui_container(
        clsid      = 'SAPGUI.CONTAINERCTRL.1'
        parent     = cl_gui_container=>default_screen
        lifetime   = mv_lifetime
        autoalign  = abap_true
    ).
  ENDMETHOD.

  METHOD create_control_toolbar.
    zcl_uitb_gui_helper=>create_control_toolbar(
      EXPORTING
        io_parent    = io_parent
        iv_mode      = iv_mode
        if_show_sash = if_show_sash
        it_button    = it_button
      IMPORTING
        eo_toolbar   = eo_toolbar
        eo_client    = eo_client
    ).

    register_toolbar( eo_toolbar ).
  ENDMETHOD.

  METHOD on_dynpro_before_output.
*.. initialize the controls on first call
    IF mf_screen_is_live = abap_false.
      TRY.
          create_container( ).
          create_content( mo_container ).
          mf_screen_is_live = abap_true.
          mf_visible = abap_true.
        CATCH zcx_uitb_gui_exception INTO DATA(lx_gui_error).
          MESSAGE lx_gui_error->get_text( ) TYPE 'X'.
      ENDTRY.
    ENDIF.

    do_before_dynpro_output( io_callback = eo_callback ).

  ENDMETHOD.

  METHOD on_dynpro_user_command.
    execute_command( NEW cl_ui_command(
      iv_type     = zif_uitb_gui_command=>c_command_type-dynpro
      iv_function = ev_function_id  )
    ).
  ENDMETHOD.


  METHOD on_exit.
    handle_exit_request( eo_callback ).
  ENDMETHOD.

  METHOD on_exited.
    IF mo_container IS BOUND.
      mo_container->free( EXCEPTIONS OTHERS = 1 ).
      cl_gui_cfw=>flush( ).
    ENDIF.

    CLEAR: mo_container,
           mf_screen_is_live,
           mf_visible.

    do_after_exit( ).
  ENDMETHOD.

  METHOD register_toolbar.
    SET HANDLER:
      on_toolbar_dropdown FOR io_toolbar,
      on_toolbar_function FOR io_toolbar.
  ENDMETHOD.

  METHOD on_toolbar_dropdown.
    DATA(lo_command) = NEW cl_ui_command(
      iv_type     = zif_uitb_gui_command=>c_command_type-request_menu
      iv_function = fcode
    ).
    execute_command( lo_command ).

    IF lo_command->mo_menu IS BOUND.
      sender->track_context_menu(
        EXPORTING
          context_menu = lo_command->mo_menu
          posx         = posx
          posy         = posy
        EXCEPTIONS
          ctmenu_error = 1
          OTHERS       = 2
      ).
      IF sy-subrc <> 0.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD on_toolbar_function.
    execute_command( NEW cl_ui_command(
      iv_type     = zif_uitb_gui_command=>c_command_type-normal
      iv_function = fcode  )
    ).
  ENDMETHOD.

  METHOD handle_exit_request.
    RETURN.
  ENDMETHOD.

  METHOD do_before_dynpro_output.
    RETURN.
  ENDMETHOD.

  METHOD do_after_exit.
    RETURN.
  ENDMETHOD.

  METHOD trigger_command.
    execute_command( io_command = NEW cl_ui_command(
      iv_type     = zif_uitb_gui_command=>c_command_type-normal
      iv_function = iv_function
    ) ).
  ENDMETHOD.


ENDCLASS.
