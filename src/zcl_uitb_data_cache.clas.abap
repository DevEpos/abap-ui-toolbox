CLASS zcl_uitb_data_cache DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">Get current instance of data cache</p>
    "!
    "! Each program has its own cache, so the program id is necessary to retrieve
    "! the correct instance
    "!
    "! @parameter iv_repid | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter rr_instance | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS get_instance
      IMPORTING
        !iv_repid          TYPE sy-repid
      RETURNING
        VALUE(rr_instance) TYPE REF TO zcl_uitb_data_cache .
    "! <p class="shorttext synchronized" lang="en">Retrieve data reference for given name</p>
    "!
    "! @parameter iv_registered_name | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter rr_data_ref | <p class="shorttext synchronized" lang="en"></p>
    METHODS get_data_ref
      IMPORTING
        !iv_registered_name TYPE string
      RETURNING
        VALUE(rr_data_ref)  TYPE REF TO data .
    "! <p class="shorttext synchronized" lang="en">Updates value of object reference</p>
    "!
    "! @parameter iv_registered_name | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter ir_new_object_ref | <p class="shorttext synchronized" lang="en"></p>
    METHODS update_object_ref
      IMPORTING
        !iv_registered_name TYPE string
        !ir_new_object_ref  TYPE REF TO object .
    "! <p class="shorttext synchronized" lang="en">Clears object reference</p>
    METHODS clear_object_ref
      IMPORTING
        !iv_registered_name TYPE string .

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA gr_instance TYPE REF TO zcl_uitb_data_cache .
    DATA:
      mv_current_repid         TYPE sy-repid,
      mf_initializing_progpool TYPE abap_bool.

    "! <p class="shorttext synchronized" lang="en">Initializes the program cache</p>
    "!
    METHODS initialize_cache.
    "! <p class="shorttext synchronized" lang="en">Set's the current report id</p>
    "!
    "! @parameter iv_repid | <p class="shorttext synchronized" lang="en"></p>
    METHODS set_current_repid
      IMPORTING
        iv_repid TYPE sy-repid .
ENDCLASS.



CLASS zcl_uitb_data_cache IMPLEMENTATION.

  METHOD clear_object_ref.
    DATA(lr_object_ref) = get_data_ref( iv_registered_name ).
    ASSIGN lr_object_ref->* TO FIELD-SYMBOL(<lr_object_ref>).
    IF sy-subrc = 0.
      clear: <lr_object_ref>.
    ENDIF.
  ENDMETHOD.


  METHOD get_data_ref.
    DATA(lv_prog_variable) = |({ mv_current_repid }){ iv_registered_name }|.

    ASSIGN (lv_prog_variable) TO FIELD-SYMBOL(<l_prog_variable>).
    IF sy-subrc = 0.
      CLEAR mf_initializing_progpool.
      rr_data_ref = REF #( <l_prog_variable> ).
    ELSE.
*.... If the variable was still not found, there seems to be a programming error
      IF mf_initializing_progpool = abap_true.
        MESSAGE |Variable { iv_registered_name } does not exist in the Program { mv_current_repid }| TYPE 'A'.
      ENDIF.
*.... Try to initialize program pool
      initialize_cache( ).
      mf_initializing_progpool = abap_true.
      rr_data_ref = get_data_ref( iv_registered_name ).
    ENDIF.

  ENDMETHOD.


  METHOD get_instance.
    IF gr_instance IS INITIAL.
      gr_instance = NEW #( ).
    ENDIF.

    gr_instance->set_current_repid( iv_repid ).
    rr_instance = gr_instance.
  ENDMETHOD.


  METHOD initialize_cache.
    PERFORM ('INIT') IN PROGRAM (mv_current_repid).
  ENDMETHOD.


  METHOD set_current_repid.
    mv_current_repid = iv_repid.
  ENDMETHOD.


  METHOD update_object_ref.
    DATA(lr_object_ref) = get_data_ref( iv_registered_name ).
    ASSIGN lr_object_ref->* TO FIELD-SYMBOL(<lr_object_ref>).
    IF sy-subrc = 0.
      <lr_object_ref> ?= ir_new_object_ref.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
