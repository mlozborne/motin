WITH LayoutPkg; USE LayoutPkg;

PACKAGE SSIPkg IS
   TASK TYPE SSITaskType IS
      ENTRY SetLayout (L : IN LayoutManagerAccess);
   END SSITaskType;
END SSIPkg;
