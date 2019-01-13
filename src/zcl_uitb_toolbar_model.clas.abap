"! <p class="shorttext synchronized" lang="en">Toolbar Model</p>
CLASS zcl_uitb_toolbar_model DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_uitb_toolbar_events .
    INTERFACES zif_uitb_c_toolbar_functions .

    ALIASES collapse_all
      FOR zif_uitb_c_toolbar_functions~collapse_all .
    ALIASES expand_all
      FOR zif_uitb_c_toolbar_functions~expand_all .
    ALIASES find
      FOR zif_uitb_c_toolbar_functions~find .
    ALIASES find_more
      FOR zif_uitb_c_toolbar_functions~find_more .

    "! <p class="shorttext synchronized" lang="en">Adds a new toolbar button</p>
    METHODS add_button
      IMPORTING
        !iv_fcode     TYPE ui_func
        !iv_icon      TYPE c OPTIONAL
        !if_disabled  TYPE abap_bool OPTIONAL
        !iv_butn_type TYPE tb_btype DEFAULT cntb_btype_button
        !iv_text      TYPE text40 OPTIONAL
        !iv_quickinfo TYPE iconquick OPTIONAL
        !if_checked   TYPE c OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Add button menu</p>
    "!
    "! @parameter iv_function | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter ir_menu | <p class="shorttext synchronized" lang="en"></p>
    METHODS add_button_menu
      IMPORTING
        iv_function TYPE ui_func
        ir_menu     TYPE REF TO cl_ctmenu.
    "! <p class="shorttext synchronized" lang="en">Add new toolbar buttons</p>
    METHODS add_buttons
      IMPORTING
        !it_buttons TYPE ttb_button .
    "! <p class="shorttext synchronized" lang="en">Adds expand/collapse buttons to the toolbar</p>
    METHODS add_expander_buttons .
    "! <p class="shorttext synchronized" lang="en">Adds Search buttons to the toolbar</p>
    METHODS add_search_buttons .
    "! <p class="shorttext synchronized" lang="en">Adds a separator to the toolbar</p>
    METHODS add_separator .
    "! <p class="shorttext synchronized" lang="en">Connect the model to a toolbar control</p>
    METHODS connect_control
      IMPORTING
        !ir_toolbar TYPE REF TO cl_gui_toolbar .
    "! <p class="shorttext synchronized" lang="en">Get list of all buttons of the toolbar</p>
    METHODS get_list
      RETURNING
        VALUE(result) TYPE ttb_button .
    "! <p class="shorttext synchronized" lang="en">Refreshes the ui -&gt; after connecting</p>
    METHODS refresh_ui .
    "! <p class="shorttext synchronized" lang="en">Change button icon</p>
    "!
    "! @parameter iv_function | <p class="shorttext synchronized" lang="en">function code of the button</p>
    "! @parameter iv_icon | <p class="shorttext synchronized" lang="en"></p>
    METHODS set_button_icon
      IMPORTING
        !iv_function TYPE ui_func
        !iv_icon     TYPE c .
    "! <p class="shorttext synchronized" lang="en">Update button menu</p>
    "!
    "! @parameter iv_function | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter ir_menu | <p class="shorttext synchronized" lang="en"></p>
    METHODS set_button_menu
      IMPORTING
        iv_function TYPE ui_func
        ir_menu     TYPE REF TO cl_ctmenu.

    "! <p class="shorttext synchronized" lang="en">Set button state</p>
    "!
    "! @parameter iv_function | <p class="shorttext synchronized" lang="en">function code of the button</p>
    "! @parameter if_enabled | <p class="shorttext synchronized" lang="en">Enable the button?</p>
    "! @parameter if_checked | <p class="shorttext synchronized" lang="en">Set the button to checked?</p>
    METHODS set_button_state
      IMPORTING
        !iv_function TYPE ui_func
        !if_enabled  TYPE abap_bool OPTIONAL
        !if_checked  TYPE abap_bool OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_tb_button.
        INCLUDE TYPE stb_button.
    TYPES: state_changed TYPE abap_bool.
    TYPES: info_changed TYPE abap_bool.
    TYPES: deleted TYPE abap_bool.
    TYPES: END OF ty_tb_button .

    TYPES: BEGIN OF ty_tb_button_menu.
        INCLUDE TYPE stb_btnmnu.
    TYPES: changed TYPE abap_bool.
    TYPES: END OF ty_tb_button_menu.

    DATA:
      mt_buttons TYPE STANDARD TABLE OF ty_tb_button WITH EMPTY KEY .
    DATA mt_button_menu TYPE STANDARD TABLE OF ty_tb_button_menu WITH EMPTY KEY.
    DATA mf_changed TYPE abap_bool .
    DATA mf_state_changed TYPE abap_bool .
    DATA mr_toolbar TYPE REF TO cl_gui_toolbar .

    "! <p class="shorttext synchronized" lang="en">Handler for GUI Toolbar Button click</p>
    METHODS on_toolbar_button
          FOR EVENT function_selected OF cl_gui_toolbar
      IMPORTING
          !sender
          !fcode .
    "! <p class="shorttext synchronized" lang="en">Handler for GUI Toolbar Dropdown click</p>
    METHODS on_toolbar_dropdown
          FOR EVENT dropdown_clicked OF cl_gui_toolbar
      IMPORTING
          !fcode
          !posx
          !posy .
ENDCLASS.



CLASS zcl_uitb_toolbar_model IMPLEMENTATION.

  METHOD add_button_menu.
    mt_button_menu = VALUE #(
      BASE mt_button_menu
      ( function = iv_function
        ctmenu   = ir_menu    )
    ).
    mf_changed = abap_true.
  ENDMETHOD.

  METHOD add_button.
    mt_buttons = VALUE #(
      BASE mt_buttons
      ( butn_type = iv_butn_type
        checked   = if_checked
        disabled  = if_disabled
        function  = iv_fcode
        icon      = iv_icon
        quickinfo = iv_quickinfo
        text      = iv_text       )
    ).
    mf_changed = abap_true.
  ENDMETHOD.


  METHOD add_buttons.
    mt_buttons = VALUE #( BASE mt_buttons ( LINES OF CORRESPONDING #( it_buttons ) ) ).
    mf_changed = abap_true.
  ENDMETHOD.


  METHOD add_expander_buttons.
    mt_buttons = VALUE #(
      BASE mt_buttons
      ( butn_type = cntb_btype_button
        function  = expand_all
        icon      = icon_expand_all
        quickinfo = 'Expand All'(002) )
      ( butn_type = cntb_btype_button
        function  = collapse_all
        icon      = icon_collapse_all
        quickinfo = 'Collapse All'(001) )
    ).
    mf_changed = abap_true.
  ENDMETHOD.


  METHOD add_search_buttons.
    mt_buttons = VALUE #(
      BASE mt_buttons
      ( butn_type = cntb_btype_button
        function  = find
        icon      = icon_search
        quickinfo = 'Find'(003) )
      ( butn_type = cntb_btype_button
        function  = find_more
        icon      = icon_search_next
        quickinfo = 'Find next'(004) )
    ).
    mf_changed = abap_true.
  ENDMETHOD.


  METHOD add_separator.
    mt_buttons = VALUE #( BASE mt_buttons ( butn_type = cntb_btype_sep ) ).
    mf_changed = abap_true.
  ENDMETHOD.


  METHOD connect_control.
    DATA: lt_events TYPE cntl_simple_events.
    mr_toolbar = ir_toolbar.

    lt_events = VALUE #( ( eventid = cl_gui_toolbar=>m_id_function_selected appl_event = abap_true )
                         ( eventid = cl_gui_toolbar=>m_id_dropdown_clicked  appl_event = abap_true ) ).

    mr_toolbar->set_registered_events( events = lt_events ).

    SET HANDLER:
        on_toolbar_button FOR mr_toolbar,
        on_toolbar_dropdown FOR mr_toolbar.
  ENDMETHOD.


  METHOD get_list.
    result = mt_buttons.
  ENDMETHOD.


  METHOD on_toolbar_button.
    RAISE EVENT zif_uitb_toolbar_events~function_selected
      EXPORTING
        ev_fcode = fcode.
  ENDMETHOD.


  METHOD on_toolbar_dropdown.
    RAISE EVENT zif_uitb_toolbar_events~dropdown_clicked
      EXPORTING
        ev_fcode = fcode.
  ENDMETHOD.


  METHOD refresh_ui.

*.. Toolbar changed so much, that all buttons are updated
    IF mf_changed = abap_true.
      mr_toolbar->delete_all_buttons( ).
      mr_toolbar->add_button_group( CORRESPONDING #( mt_buttons ) ).
      mr_toolbar->assign_static_ctxmenu_table( CORRESPONDING #( mt_button_menu ) ).
      CLEAR: mf_changed.
    ELSE.
*.... Only update those buttons that really need an update on the ui
      LOOP AT mt_buttons ASSIGNING FIELD-SYMBOL(<ls_button>) WHERE state_changed = abap_true
                                                                OR info_changed = abap_true.

        IF <ls_button>-info_changed = abap_true.
          mr_toolbar->set_button_info(
            EXPORTING
              fcode            = <ls_button>-function
              icon             = <ls_button>-icon
              text             = <ls_button>-text
              quickinfo        = <ls_button>-quickinfo
            EXCEPTIONS
              cntl_error       = 1
              cntb_error_fcode = 2
              OTHERS           = 3
          ).
          CLEAR <ls_button>-info_changed.
        ELSE.
          mr_toolbar->set_button_state(
            EXPORTING
              enabled          = xsdbool( <ls_button>-disabled = abap_false )
              checked          = <ls_button>-checked
              fcode            = <ls_button>-function
            EXCEPTIONS
              cntl_error       = 1
              cntb_error_fcode = 2
              OTHERS           = 3
          ).
          CLEAR <ls_button>-state_changed.
        ENDIF.
      ENDLOOP.

      LOOP AT mt_button_menu ASSIGNING FIELD-SYMBOL(<ls_button_menu>) WHERE changed = abap_true.
        mr_toolbar->set_static_ctxmenu(
          EXPORTING
            fcode                = <ls_button_menu>-function
            ctxmenu              = <ls_button_menu>-ctmenu
          EXCEPTIONS
            ctmenu_error         = 1
            cntl_error           = 2
            cntb_error_parameter = 3
            OTHERS               = 4
        ).
        CLEAR <ls_button_menu>-changed.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD set_button_icon.
    ASSIGN mt_buttons[ function = iv_function ] TO FIELD-SYMBOL(<ls_button>).
    CHECK sy-subrc = 0.

    IF iv_icon <> <ls_button>-icon.
      <ls_button>-icon = iv_icon.
      <ls_button>-info_changed = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD set_button_menu.
    CHECK line_exists( mt_buttons[ function = iv_function ] ).

    ASSIGN mt_button_menu[ function = iv_function ] TO FIELD-SYMBOL(<ls_button_menu>).
    IF sy-subrc <> 0.
      INSERT VALUE #(
        function = iv_function
      ) INTO TABLE mt_button_menu ASSIGNING <ls_button_menu>.
    ENDIF.

    <ls_button_menu>-changed = abap_true.
    <ls_button_menu>-ctmenu = ir_menu.
  ENDMETHOD.


  METHOD set_button_state.
    ASSIGN mt_buttons[ function = iv_function ] TO FIELD-SYMBOL(<ls_button>).
    CHECK sy-subrc = 0.

    IF if_checked IS SUPPLIED AND
       <ls_button>-checked <> if_checked.
      <ls_button>-checked = if_checked.
      <ls_button>-state_changed = abap_true.
    ENDIF.

    IF if_enabled IS SUPPLIED.
      DATA(lf_disabled) = xsdbool( if_enabled = abap_false ).
      IF lf_disabled <> <ls_button>-disabled.
        <ls_button>-disabled = lf_disabled.
        <ls_button>-state_changed = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
