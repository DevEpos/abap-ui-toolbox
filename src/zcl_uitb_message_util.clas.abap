"! <p class="shorttext synchronized" lang="en">Message Utility</p>
CLASS zcl_uitb_message_util DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Splits text into sy-msg variables</p>
      split_string_to_symsg
        IMPORTING
          iv_text TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_uitb_message_util IMPLEMENTATION.

  METHOD split_string_to_symsg.

    DATA: lv_offset TYPE i.

    DATA(lv_rest_text) = iv_text.

    DATA(lv_msgv1) = lv_rest_text.
    SHIFT lv_rest_text LEFT BY 50 PLACES.
    DATA(lv_msgv2) = lv_rest_text.
    SHIFT lv_rest_text LEFT BY 50 PLACES.
    DATA(lv_msgv3) = lv_rest_text.
    SHIFT lv_rest_text LEFT BY 50 PLACES.
    DATA(lv_msgv4) = lv_rest_text.

    IF strlen( lv_rest_text ) > 50.
      FIND ALL OCCURRENCES OF REGEX '.\s.' IN SECTION LENGTH 47 OF lv_msgv4 MATCH OFFSET lv_offset.
      IF sy-subrc = 0.
        lv_offset = lv_offset + 1.
        lv_msgv4 = lv_msgv4(lv_offset).

        lv_msgv4 = |{ lv_msgv4 }...|.
      ENDIF.
    ENDIF.

    MESSAGE e001(00) WITH lv_msgv1 lv_msgv2 lv_msgv3 lv_msgv4 INTO DATA(lv_msg) ##NEEDED.
  ENDMETHOD.

ENDCLASS.
