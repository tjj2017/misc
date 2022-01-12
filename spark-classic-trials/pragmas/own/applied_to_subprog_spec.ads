package Applied_To_Subprog_Spec is
   procedure P;
   pragma Classic_Own (((Plain => V) with Own_Type => Integer));
end Applied_To_Subprog_Spec;
