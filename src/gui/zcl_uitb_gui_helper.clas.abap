CLASS zcl_uitb_gui_helper DEFINITION
  PUBLIC
  CREATE PRIVATE.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Creates Menu from button table</p>
    "!
    CLASS-METHODS create_menu
      IMPORTING
        it_buttons     TYPE ttb_button
      RETURNING
        VALUE(ro_menu) TYPE REF TO cl_ctmenu.
    "! <p class="shorttext synchronized" lang="en">Creates control toolbar</p>
    "!
    CLASS-METHODS create_control_toolbar
      IMPORTING
        io_parent       TYPE REF TO cl_gui_container
        if_show_sash    TYPE abap_bool OPTIONAL
        iv_toolbar_size TYPE i OPTIONAL
        iv_mode         TYPE i DEFAULT cl_gui_toolbar=>m_mode_horizontal
        !it_button      TYPE ttb_button OPTIONAL
      EXPORTING
        !eo_toolbar     TYPE REF TO cl_gui_toolbar
        !eo_client      TYPE REF TO cl_gui_container.

    "! <p class="shorttext synchronized" lang="en">Creates container for a control</p>
    "!
    CLASS-METHODS create_control_container
      IMPORTING
        !io_parent          TYPE REF TO cl_gui_container OPTIONAL
        iv_lifetime         TYPE i DEFAULT cntl_lifetime_imode
        !iv_area            TYPE screen-name OPTIONAL
        !iv_style           TYPE i OPTIONAL
        !iv_type            TYPE i DEFAULT cl_gui_container=>container_type_simple
          PREFERRED PARAMETER io_parent
      RETURNING
        VALUE(ro_container) TYPE REF TO cl_gui_container.

    "! <p class="shorttext synchronized" lang="en">Checks if given control has the focus</p>
    CLASS-METHODS has_focus
      IMPORTING
        io_control      TYPE REF TO cl_gui_control
      RETURNING
        VALUE(rf_focus) TYPE abap_bool.

    "! <p class="shorttext synchronized" lang="en">Sets focus to control</p>
    CLASS-METHODS set_focus
      IMPORTING
        io_control TYPE REF TO cl_gui_control.

    "! <p class="shorttext synchronized" lang="en">Returns 'X' if belize theme is active</p>
    CLASS-METHODS is_belize_theme
      RETURNING
        VALUE(rf_is_belize) TYPE abap_bool.

    "! <p class="shorttext synchronized" lang="en">Returns default width/height for control container</p>
    "! Returns the optimum width/height of a container that contains only GUI Buttons
    "! dependent on the current theme. <br/>
    CLASS-METHODS get_default_ctrl_height
      RETURNING
        VALUE(rv_size) TYPE i.
    "! <p class="shorttext synchronized" lang="en">Returns default width/height for control container</p>
    "! Returns the optimum width/height of a container that contains only GUI Buttons
    "! dependent on the current theme. <br/>
    CLASS-METHODS get_default_ctrl_width
      RETURNING
        VALUE(rv_size) TYPE i.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA go_temp_ctrl TYPE REF TO cl_gui_control.
    CLASS-DATA gv_sap_gui_theme TYPE string.
ENDCLASS.



CLASS zcl_uitb_gui_helper IMPLEMENTATION.

  METHOD create_control_toolbar.

    DATA: lo_container TYPE REF TO cl_gui_container,
          lt_event     TYPE cntl_simple_events,
          ls_event     TYPE cntl_simple_event.

    DATA(lv_toolbar_size) = iv_toolbar_size.

    IF lv_toolbar_size IS INITIAL.
      lv_toolbar_size = SWITCH #( iv_mode
        WHEN zcl_uitb_gui_splitter_cont=>c_mode-cols THEN get_default_ctrl_width( )
        WHEN zcl_uitb_gui_splitter_cont=>c_mode-rows THEN get_default_ctrl_height( ) ).
    ENDIF.

    DATA(lo_splitter) = NEW zcl_uitb_gui_splitter_cont(
      iv_elements  = 2
      iv_size      = |{ lv_toolbar_size }:*|
      io_parent    = io_parent ).

    lo_splitter->set_sash_properties(
      iv_index   = 1
      if_visible = if_show_sash
      if_movable = abap_false ).

    lo_container = lo_splitter->get_container( iv_index = 1 ).
*.. Create the toolbar object
    CREATE OBJECT eo_toolbar
      EXPORTING
        parent             = lo_container
        display_mode       = iv_mode
      EXCEPTIONS
        cntl_install_error = 1
        cntl_error         = 2
        cntb_wrong_version = 3
        OTHERS             = 4.
    IF sy-subrc NE 0.
      zcx_uitb_gui_exception=>raise_from_sy( ).
    ENDIF.
    IF it_button IS NOT INITIAL.
      eo_toolbar->add_button_group(
        EXPORTING
          data_table       = it_button
        EXCEPTIONS
          dp_error         = 1
          cntb_error_fcode = 2
          OTHERS           = 3
      ).
    ENDIF.
    IF sy-subrc NE 0.
      zcx_uitb_gui_exception=>raise_from_sy( ).
    ENDIF.

*.. request toolbar events
    lt_event = VALUE #(
      ( eventid = cl_gui_toolbar=>m_id_function_selected )
      ( eventid = cl_gui_toolbar=>m_id_dropdown_clicked  )
    ).

*.. Return respective controls back to caller
    eo_toolbar->set_registered_events( lt_event ).
    eo_client = lo_splitter->get_container( 2 ).

  ENDMETHOD.

  METHOD create_menu.
    ro_menu = NEW #( ).

    LOOP AT it_buttons ASSIGNING FIELD-SYMBOL(<ls_button>).
      IF <ls_button>-butn_type = cntb_btype_sep.
        ro_menu->add_separator( ).
      ELSE.
        ro_menu->add_function(
            fcode = <ls_button>-function
            text  = <ls_button>-text
            icon  = |{ <ls_button>-icon }|
        ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD create_control_container.

*.. Create container
    IF io_parent IS BOUND.
      CREATE OBJECT ro_container
        TYPE
        cl_gui_simple_container
        EXPORTING
          parent            = io_parent
          lifetime          = iv_lifetime
          autoalign         = abap_true
        EXCEPTIONS
          cntl_error        = 1
          cntl_system_error = 2
          OTHERS            = 3.
      IF sy-subrc NE 0.
        zcx_uitb_gui_exception=>raise_from_sy( ).
      ENDIF.
    ELSEIF iv_area IS NOT INITIAL.
      CREATE OBJECT ro_container
        TYPE
        cl_gui_custom_container
        EXPORTING
          container_name    = iv_area
          lifetime          = iv_lifetime
        EXCEPTIONS
          cntl_error        = 1
          cntl_system_error = 2
          OTHERS            = 3.
      IF sy-subrc NE 0.
        zcx_uitb_gui_exception=>raise_from_sy( ).
      ENDIF.
    ELSE.
      CASE iv_type.
        WHEN cl_gui_container=>container_type_docking.
          CREATE OBJECT ro_container
            TYPE
            cl_gui_docking_container
            EXPORTING
              style             = iv_style
              lifetime          = iv_lifetime
            EXCEPTIONS
              cntl_error        = 1
              cntl_system_error = 2
              OTHERS            = 3.
          IF sy-subrc NE 0.
            zcx_uitb_gui_exception=>raise_from_sy( ).
          ENDIF.
        WHEN cl_gui_container=>container_type_dialogbox.
          CREATE OBJECT ro_container
            TYPE
            cl_gui_docking_container
            EXPORTING
              parent            = io_parent
              lifetime          = iv_lifetime
            EXCEPTIONS
              cntl_error        = 1
              cntl_system_error = 2
              OTHERS            = 3.
          IF sy-subrc NE 0.
            zcx_uitb_gui_exception=>raise_from_sy( ).
          ENDIF.
        WHEN OTHERS.
          CREATE OBJECT ro_container
            TYPE
            cl_gui_container
            EXPORTING
              clsid                   = 'SAPGUI.CONTAINERCTRL.1'
              parent                  = cl_gui_container=>default_screen
              lifetime                = iv_lifetime
              no_autodef_progid_dynnr = abap_false
              autoalign               = abap_true
            EXCEPTIONS
              OTHERS                  = 1.
          IF sy-subrc NE 0.
            zcx_uitb_gui_exception=>raise_from_sy( ).
          ENDIF.
      ENDCASE.
    ENDIF.

  ENDMETHOD.

  METHOD has_focus.
    CHECK io_control IS BOUND.

    cl_gui_control=>get_focus(
      IMPORTING
        control           = go_temp_ctrl
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3 ).
    IF sy-subrc = 0.
      rf_focus = xsdbool( go_temp_ctrl = io_control ).
    ENDIF.
  ENDMETHOD.

  METHOD set_focus.
    CHECK io_control IS BOUND.

    cl_gui_container=>set_focus(
      EXPORTING
        control           = io_control
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3 ).
  ENDMETHOD.

  METHOD is_belize_theme.
    IF gv_sap_gui_theme IS INITIAL.
      cl_gui_resources=>get_themename( IMPORTING themename = gv_sap_gui_theme ).
    ENDIF.

    rf_is_belize = xsdbool(
      gv_sap_gui_theme = 'Belize' OR
      gv_sap_gui_theme CP 'sap_belize*'  ).
  ENDMETHOD.

  METHOD get_default_ctrl_height.
    rv_size = COND #( WHEN is_belize_theme( ) THEN 28 ELSE 21 ).
  ENDMETHOD.

  METHOD get_default_ctrl_width.
    rv_size = COND #( WHEN is_belize_theme( ) THEN 28 ELSE 26 ).
  ENDMETHOD.

ENDCLASS.
