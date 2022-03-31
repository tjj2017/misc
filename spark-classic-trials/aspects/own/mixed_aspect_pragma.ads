package Mixed_Aspect_Pragma
with Classic_Own => ((Plain => V))
is
   pragma Classic_Initializes ((V));
   V : Integer;
private
   PV : Integer;
end Mixed_Aspect_Pragma;
