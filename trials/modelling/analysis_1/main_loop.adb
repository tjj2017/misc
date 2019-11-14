with Interface_Module;
procedure Main_Loop is
begin
   loop
      Interface_Module.Read_Inputs;

      null;

      Interface_Module.Write_Outputs;
   end loop;
end Main_Loop;



