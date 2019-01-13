*"* use this source file for any macro definitions you need
*"* in the implementation part of the class
define assign_table.
  field-SYMBOLS: <lt_&2> type table.
  assign &1->* to <lt_&2>.
end-OF-DEFINITION.

define assign_struct.
  field-symbols: <ls_&2> type any.

  assign &1->* to <ls_&2>.
end-of-definition.

define create_line.
  data: lr_s_&1 type ref to data.
  FIELD-symbols: <ls_&1> type any.

  create data lr_s_&1 type handle mr_line_type.

  assign lr_s_&1->* to <ls_&1>.

end-of-definition.

define create_table.
  data: lr_t_&1 type ref to data.
  FIELD-symbols: <lt_&1> type any.

  create data lr_t_&1 type handle mr_table_type.

  assign lr_t_&1->* to <lt_&1>.

end-of-definition.
