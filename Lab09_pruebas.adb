-------------------------------------------------------
--                                                   --
--      PROGRAMA DE PRUEBAS - LABORATORIO 09         --
--       9 DE NOVIEMBRE DEL 2021 // DONOSTIA         --
--           CURSO 2021/2022 - UPV/EHU               --
--                                                   --
--          MODIFICACIONES: NICOLAS AGUADO           --
--                  nico@nico.eus                    --
--                                                   --
-------------------------------------------------------



with Laboratorio09; use Laboratorio09;
with lab09_escenarios; use Lab09_escenarios;
with  Ada.Text_IO, Ada.Integer_Text_IO, Ada.Float_Text_IO;
use  Ada.Text_IO, Ada.Integer_Text_IO, Ada.Float_Text_IO;
procedure Lab09_Pruebas is
   -- Donado por Oihan Irastorza [curso 2020-2021]
   -- programa auxiliar para representar por consola un objeto T_Datos_Pluviometricos
   procedure mostrar_datos_pluviometricos (Lluvia: T_Datos_Pluviometricos) IS
   BEGIN
      put("-- Datos pluviometricos --");
      for aa in Lluvia'range loop -- iterar sobre anno
         put("Anno: " & aa'img);
         new_line;
         for mm in Lluvia(aa)'range loop -- iterar sobre mes
            put("  Mes: " & mm'img);
            new_line;
            for dd in Lluvia(aa)(mm)'range loop -- iterar sobre dia
               put("    Dia: " & dd'img & ". Valor --> " & Lluvia(aa)(mm)(dd)'img);
               new_line;
            end loop;
            new_line(2);
         end loop;
         new_line(5);
      end loop;
      put("-- Final datos pluviometricos --");
   END mostrar_datos_pluviometricos;

   -- programa auxiliar para representar por consola un objeto T_Estatica_Carreteras
   procedure mostrar_carreteras(L: T_Estatica_Carreteras; inicio: Integer := 1; final: Integer := Max_Carreteras) is
   begin
      for crt in inicio..final loop
         Put("   Carretera " & crt'img);
         new_line;
         Put_Line("      Codigo: " & L.Ctras(crt).Codigo);
         Put("      Peaje_km: "); put(L.Ctras(crt).Peaje_km, 1, 2, 0);
         new_line(2);
      end loop;
   end mostrar_carreteras;

   -- programa auxiliar para representar por consola un objeto T_Punto (hasta centinela si lo hubiese)
   procedure mostrar_puntos_por_carretera(L: T_Estatica_Carreteras) is
      ptos: V_puntos(1..max_puntos);
      ptos_cont: Natural;
   begin
      for ctra in L.Ctras'First..L.Ctras'First+L.Cont-1 loop
         put("   Carretera " & L.Ctras(ctra).Codigo);
         new_line;
         ptos := L.Ctras(ctra).Trazado.Ptos;
         ptos_cont := L.Ctras(ctra).Trazado.Num;
         for pto in ptos'First..ptos'First+ptos_cont-1  loop
            put("      ("); put(ptos(pto).X, 1, 5, 0); put(", "); put(ptos(pto).Y, 1, 5, 0); put(")");
            new_line;
         end loop;
         new_line;
      end loop;
   end mostrar_puntos_por_carretera;

   -- programa auxiliar para representar por consola un objeto T_Estatica_Puntos, hasta encontrarse con CENTINELA
   procedure mostrar_puntos(L: T_Estatica_Puntos) is
      k: Integer := L.Ptos'First;
   begin
      while L.Ptos(k) /= CENTINELA loop
         put("("); put(L.Ptos(k).X, 1, 1, 0); put(", "); put(L.Ptos(k).Y, 1, 1, 0); put(")");
         k := k + 1;
      end loop;
   end Mostrar_Puntos;

   -- programa auxiliar para representar por consola un objeto T_Urbanizacion
   procedure mostrar_urbanizacion(U: T_Urbanizacion) is
   begin
      put("-- Mostrar urbanizacion --");
      new_line;

      for bloque in U'range loop -- iterar sobre cada bloque
         put("Bloque: " & bloque'img);
         new_line;
         for planta in U(bloque)'range loop
            put("   Planta: " & planta'img);
            new_line;
            for apart in U(bloque)(planta).Apartamentos'First..U(bloque)(planta).Num_Apartamentos loop
               Put_Line("      Apartamento: " & apart'img);
                  Put_Line("         Propietario: " & U(bloque)(planta).Apartamentos(apart).Propietario_Actual.Dni'img);
                  Put_Line("         Precio: " & U(bloque)(planta).Apartamentos(apart).Precio'img);
                  new_line;
            end loop;
            new_line;
         end loop;
         new_line;
      end loop;

      new_line(2);
      put("-- Final mostrar urbanizacion --");
   end mostrar_urbanizacion;

   -- programa auxiliar para representar por consola un objeto T_Ficha
   procedure mostrar_ficha(F: T_Ficha) is
   begin
      for i in F'range(1) loop
         for j in F'range(2) loop
            put(F(i, j), 2);
         end loop;
         new_line;
      end loop;
   end mostrar_ficha;

   V1, V2:T_Vector_Enteros(1..10);
   V10:T_Vector_Enteros(5..15);
   comunesCont:Natural := 0;
   N,Clave,Num:T_Vector_Enteros(1..5);
   N3:T_Vector_Enteros(1..3);
   L: T_Lista_Estatica;
   Num2:Integer;
   J:Integer;
   C: T_Comunidad;
   R: T_Rascacielos;
   condicionxd: boolean;
   Elpepe: T_Vecino;
   E:T_Edificio;
   Consumo_electrico: Float;
   Consumo_gas: T_Consumo_Medio_Manos;
   V: T_Vivienda;
   U: T_Urbanizacion;
   Lluvia : T_Datos_Pluviometricos;
   F, F1, F2: T_Fecha;
   LC, Peaje: T_Estatica_Carreteras;
   -- Usado para vaciar un objeto Peajes despues de cada caso de prueba
   L_vacia: constant T_Estatica_Carreteras := (
      Ctras => (
         others => ( (others => ' '), (others => ' '), (others => ' '), 0.0, ( (others => (0.0, 0.0)), 0) )
      ),
      Cont => 0
      );
   LP: T_Estatica_Puntos;
   P: T_Puzle;
   FF, CC: Integer;
   ficha: T_Ficha;


begin

   -----------------------
   -- Insertar_En_Medio --
   -----------------------
   Put_Line("Si L= [4, 5, 3, 6] y N= 9 debería quedar [4, 5, 9, 3, 6]");
   L.Dato:= (4, 5, 3, 6, others => Integer'First);
   L.cont:= 4;
   Num2:= 9;
   Insertar_En_Medio(L, Num2);
   J:= l.Dato'First;
   while J < (L.cont+l.Dato'First) loop
      Put(L.Dato(J));
      J:= J+1;
   end loop;
   New_Line(2);

   -----------------------
   -- Borrar_Intermedio --
   -----------------------
   Put_Line("Si L= [4, 5, 3, 6, 2] debería quedar [4, 5, 6, 2]");
   L.Dato:= (4, 5, 3, 6, 2, others => Integer'First);
   L.cont:= 5;
   Borrar_Intermedio(L);
   J:= l.Dato'First;
   while J < (L.cont+l.Dato'First) loop
      Put(L.Dato(J));
      J:= J+1;
   end loop;
   New_Line(2);
   --------------------
      -- Encriptar --
   --------------------
   Put_Line("Si N es (8,7,5,3,9) y Clave es (2,4,1,5,3), el resultado es (5,8,9,7,3)");
   N:= (8,7,5,3,9); Clave:= (2,4,1,5,3);
   Encriptar(N, Clave, Num);
   for I in Num'Range loop
      Put(Num(I));
   end loop;
   New_Line;

   Put_Line("Si N es(5,7,6) y Clave es (2,4,1,5,3), el resultado es (5,0,6,0,7)");
   N3:= (5,7,6); Clave:= (2,4,1,5,3);
   Encriptar(N3, Clave, Num);
   for I in Num'Range loop
      Put(Num(I));
   end loop;
   New_Line;

   New_Line;

  --------------------
      -- Comunes --
  --------------------

   Put_Line("V1 (1,2,3,4,5,6,7,8,9,10) y V2 (1,2,3,4,5,6,7,8,9,10) tienen 10 comunes");
   V1:= (1,2,3,4,5,6,7,8,9,10);
   V2:= (1,2,3,4,5,6,7,8,9,10);
   comunesCont:= Comunes(V1, V2);
   Put("Comunes contados: "); put(comunesCont'Img);
   New_Line;

   Put_Line("V1 (0,2,3,4,5,6,7,8,9,10) y V2 (1,2,3,4,5,6,7,8,9,10) tienen 9 comunes");
   V1:= (0,2,3,4,5,6,7,8,9,10);
   V2:= (1,2,3,4,5,6,7,8,9,10);
   comunesCont:= Comunes(V1, V2);
   Put("Comunes contados: "); put(comunesCont'Img);
   New_Line;

   Put_Line("V1 (1,2,3,4,5,6,7,8,9,10) y V2 (0,2,3,4,5,6,7,8,9,10) tienen 9 comunes");
   V1:= (1,2,3,4,5,6,7,8,9,10);
   V2:= (0,2,3,4,5,6,7,8,9,10);
   comunesCont:= Comunes(V1, V2);
   Put("Comunes contados: "); put(comunesCont'Img);
   New_Line;

   Put_Line("V1 (0,0,0,0,0,0,0,0,0,0) y V2 (0,2,3,4,5,6,7,8,9,10) tienen 1 comun");
   V1:= (0,0,0,0,0,0,0,0,0,0);
   V2:= (0,2,3,4,5,6,7,8,9,10);
   comunesCont:= Comunes(V1, V2);
   Put("Comunes contados: "); put(comunesCont'Img);
   New_Line;

   Put_Line("V1 (0,0,0,0,0,0,0,0,0,1) y V2 (1,2,3,4,5,6,7,8,9,10) tienen 1 comun");
   V1:= (0,0,0,0,0,0,0,0,0,1);
   V2:= (1,2,3,4,5,6,7,8,9,10);
   comunesCont:= Comunes(V1, V2);
   Put("Comunes contados: "); put(comunesCont'Img);
   New_Line;

   Put_Line("V10 (1,2,3,4,5,6,7,8,9,10,11) y V2 (1,2,3,4,5,6,7,8,9,10) tienen 10 comunes");
   V10:= (1,2,3,4,5,6,7,8,9,10,11);
   V2:= (1,2,3,4,5,6,7,8,9,10);
   comunesCont:= Comunes(V10, V2);
   Put("Comunes contados: "); put(comunesCont'Img);
   New_Line;

   New_Line;

   --------------------------------------
   -- Obtener_Num_Vecinos_por_Vivienda --
   --------------------------------------
   --Caso 1: Uno muy simple
   Put_Line("   -- Obtener_Num_Vecinos_por_Vivienda --");
   Put_Line("Casos de prueba simplificados. Para ver que ha pasado,");
   Put_line("descomentar las lineas en el fichero de pruebas");
   put("   -- Obtener_Num_Vecinos_por_Vivienda -- CASO 1");
   New_Line;
   Elpepe.nombre:= "El pepe                  ";
   Elpepe.piso:= 1;
   Elpepe.mano:= 'A';
   C:= (others => Elpepe);
   Obtener_Num_Vecinos_por_Vivienda(C,R);
--   for i in R'Range(1) loop
--      for j in R'Range(2) loop
--         put(R(i,j)); put(" ");
--      end loop;
--      new_line;
--   end loop;
   Condicionxd:= R(1,'A') = 3546;
   Put("Caso 1 Sale bien??? --->" & condicionxd'img);
   New_Line;
   put("   -- Obtener_Num_Vecinos_por_Vivienda -- CASO 2");
   New_Line;
   -- Caso 2: Datos en Lab09_Escenarios; Resultado debe ser R1
   Obtener_Num_Vecinos_por_Vivienda(C1,R);
--   for i in R'Range(1) loop
--      for j in R'Range(2) loop
--         put(R(i,j)); put(" ");
--      end loop;
--      new_line;
--   end loop;
   Condicionxd:= R1 = R;
   Put("Caso 2 Sale bien??? --->" & condicionxd'img);
   New_Line;
   put("   -- Obtener_Num_Vecinos_por_Vivienda -- CASO 3");
   New_Line;
   -- Caso 3: Datos en Lab09_Escenarios; Resultado debe ser R2
   Obtener_Num_Vecinos_por_Vivienda(C2,R);
--   for i in R'Range(1) loop
--      for j in R'Range(2) loop
--         put(R(i,j)); put(" ");
--      end loop;
--      new_line;
--   end loop;
   Condicionxd:= R2 = R;
   Put("Caso 3 Sale bien??? --->" & condicionxd'img);
   New_Line;

   put("   -- Obtener_Num_Vecinos_por_Vivienda -- CASO 4");
   New_Line;
   -- Caso 4: Datos en Lab09_Escenarios; Resultado debe ser R3
   Obtener_Num_Vecinos_por_Vivienda(C3,R);
--   for i in R'Range(1) loop
--      for j in R'Range(2) loop
--         put(R(i,j)); put(" ");
--      end loop;
--      new_line;
--   end loop;
   Condicionxd:= R3 = R;
   Put_Line("Caso 4 Sale bien??? --->" & Condicionxd'Img);
   New_Line;
   New_Line(3);


   ----------------------
   -- Obtener_Consumos --
   ----------------------
   --caso 1: uno muy simple
   put("   -- Obtener_Consumos -- CASO 1 -  ");
   V.Consumo_Electrico := 1.0; V.Consumo_Gas := 1.0; V.Habitantes := 1;
   E:= (others => (others => V));
   Obtener_Consumos(E,Consumo_electrico,Consumo_gas);
   Put("El consumo electrico medio total es: ");Put(Consumo_electrico'Img);
   Put("Y el consumo de gas medio por mano es: ");
   New_Line;
   for I in Consumo_gas'Range loop
         put("Mano ");Put(I);put(": ");put(Consumo_gas(I)'Img); put(" ");
         New_Line;
   end loop;

   New_Line(3);
   put("   -- Obtener_Consumos -- CASO 2 -  ");
   --caso 2: Datos en Lab09_Escenarios. Resultado debe ser Con_ele_E1 y Con_gas_E2
   Obtener_Consumos(E1,Consumo_Electrico,Consumo_Gas);

   Put_line("El consumo electrico medio total es " & con_ele_e1'img & " y sale ---> " & Consumo_electrico'Img);
   Put_line("Y el consumo de gas medio por mano es: ");
   for I in Consumo_gas'Range loop
      Put("Mano ");Put(I);Put(" es " & Cmm_Gas_E1(i)'img & " y sale --> ");Put(Consumo_Gas(I)'Img); Put(" ");
      New_Line;
   end loop;
   New_Line(2);

   --caso 3: Datos en Lab09_Escenarios. Resultado debe ser Con_ele_E2 y Cmm_gas_E2
   Obtener_Consumos(E2,Consumo_electrico,Consumo_gas);
   Put_line("El consumo electrico medio total es " & con_ele_e2'img & " y sale ---> " & Consumo_electrico'Img);
   Put_line("Y el consumo de gas medio por mano es: ");
   for I in Consumo_gas'Range loop
      Put("Mano ");Put(I);Put(" es " & Cmm_Gas_E2(i)'img & " y sale --> ");Put(Consumo_Gas(I)'Img); Put(" ");
      New_Line;
   end loop;
   New_Line(4);


   --------------------------
   -- Maximo Pluviometrico --
   --------------------------
   new_line(3);
   Put_Line("***** Maximo *****");
   new_line;

   -- Datos pluviometricos para todos los casos de prueba
   Lluvia:= Datos_P2;

   put("(Descomenta la call a mostrar_datos_pluviometricos() para mostrar todos los datos pluviometricos)");
   new_line;
   mostrar_datos_pluviometricos(Lluvia);

   new_line;

   -- Caso 1: Dos fechas cualesquiera
   F1 := (4, marzo, 2002);
   F2 := (16, agosto, 2018);
   Put_Line("Mes y anno con maximo de lluvias entre 4 de marzo de 2002 y 16 de agosto del 2018: mayo del 2002");
   maximo(Datos_P2, F1, F2, F);
   put(F.mes'img & " del " & F.anno'img);

   new_line(2);

   -- Caso 2: Dos fechas con el mismo anno
   F1 := (9, abril, 2009);
   F2 := (1, septiembre, 2009);
   Put_Line("Mes y anno con maximo de lluvias entre 9 de abril de 2009 y 1 de septiembre del 2009: mayo del 2009");
   maximo(Datos_P2, F1, F2, F);
   put(F.mes'img & " del " & F.anno'img);

   new_line(2);

   -- Caso 3: Primera y ultima fecha posible
   F1 := (1, enero, 2001);
   F2 := (31, diciembre, 2020);
   Put_Line("Mes y anno con maximo de lluvias entre 1 de enero de 2001 y 31 de diciembre del 2020: enero del 2001");
   maximo(Datos_P2, F1, F2, F);
   put(F.mes'img & " del " & F.anno'img);

   new_line(2);

   -- Caso 4: Mismo anno y mes
   F1 := (13, julio, 2006);
   F2 := (22, julio, 2006);
   Put_Line("Mes y anno con maximo de lluvias entre 13 de julio de 2006 y 22 de julio del 2006: julio del 2006");
   maximo(Datos_P2, F1, F2, F);
   put(F.mes'img & " del " & F.anno'img);

   new_line(2);

   -- Caso 5:
   F1 := (1, enero, 2006);
   F2 := (31, diciembre, 2020);
   Put_Line("Mes y anno con maximo de lluvias entre 1 de enero de 2006 y 31 de diciembre del 2020: enero del 2006");
   maximo(Datos_P3, F1, F2, F);
   put(F.mes'img & " del " & F.anno'img);

   new_line(2);

   -- Caso 6:
   F1 := (1, enero, 2001);
   F2 := (31, diciembre, 2020);
   Put_Line("Mes y anno con maximo de lluvias entre 1 de enero de 2001 y 31 de diciembre del 2020: enero del 2006");
   maximo(Datos_P3, F1, F2, F);
   put(F.mes'img & " del " & F.anno'img);

   new_line(2);

   -- Caso 7:
   F1 := (1, enero, 2001);
   F2 := (31, diciembre, 2005);
   Put_Line("Mes y anno con maximo de lluvias entre 1 de enero de 2001 y 31 de diciembre del 2005: mayo del 2005");
   maximo(Datos_P3, F1, F2, F);
   put(F.mes'img & " del " & F.anno'img);

   new_line(2);

   ---------------------
   -- Simplificar (1) --
   ---------------------
   new_line(3);
   Put_Line("***** Simplificar (1) *****");
   new_line;

   Put_Line("(Descomentar las calls a mostrar_carreteras() para mostrar las T_Estatica_Carretera's utilizadas)");

   new_line(2);

   -- Caso 1: 5 o mas carreteras con peaje.
   -- Resultado: S1_LC1_Res y S1_LC1_Pea
   LC := S1_LC1;

   -- * Descomentar para ver los datos de las carreteras *
   mostrar_carreteras(LC, LC.Ctras'First, LC.Ctras'Last);

   Put_Line("Se eliminan c1, c2, c3, c4, c5 y se pasan a la lista Peaje");
   Simplificar(LC, Peaje);

   new_line;

   Put_Line("Lista de carreteras actualizada: (Carreteras validas: " & LC.Cont'img & "): ");
   mostrar_carreteras(LC, LC.Ctras'First, LC.Ctras'First+LC.Cont-1);

   Put_Line("Lista de carreteras de peaje (Carreteras validas: " & Peaje.Cont'img & "): ");
   mostrar_carreteras(Peaje, Peaje.Ctras'First, Peaje.Ctras'First+Peaje.Cont-1);

   put("-------------------------");
   new_line(2);

   -- Caso 2: menos de 5 carreteras con peaje
   -- Resultado: S1_LC2_Res y S1_LC2_Pea(Lab09_escenarios)
   LC := S1_LC2;

   -- * Descomentar para ver los datos de las carreteras *
   mostrar_carreteras(LC, LC.Ctras'First, LC.Ctras'Last);

   Put_Line("Se eliminan c1 y c5");
   Simplificar(LC, Peaje);

   new_line;

   Put_Line("Lista de carreteras actualizada: (Carreteras validas: " & LC.Cont'img & "): ");
   mostrar_carreteras(LC, LC.Ctras'First, LC.Ctras'First+LC.Cont-1);

   Put_Line("Lista de carreteras de peaje (Carreteras validas: " & Peaje.Cont'img & "): ");
   mostrar_carreteras(Peaje, Peaje.Ctras'First, Peaje.Ctras'First+Peaje.Cont-1);

   put("-------------------------");
   new_line(2);

   -- Caso 3: Ninguna carretera de peaje
   -- Resultado: S1_LC3_Res y S1_LC3_Pea(Lab09_escenarios)
   LC:= S1_LC3;
   Peaje := L_vacia; -- inicializar Peaje con valores 'vacios'

   -- * Descomentar para ver los datos de las carreteras *
   mostrar_carreteras(LC, LC.Ctras'First, LC.Ctras'Last);

   Put_Line("No se borra ninguna carretera");
   Simplificar(LC, Peaje);

   new_line;

   Put_Line("Lista de carreteras actualizada: (Carreteras validas: " & LC.Cont'img & "): ");
   mostrar_carreteras(LC, LC.Ctras'First, LC.Ctras'First+LC.Cont-1);

   Put_Line("Lista de carreteras de peaje (Carreteras validas: " & Peaje.Cont'img & "): ");
   mostrar_carreteras(Peaje, Peaje.Ctras'First, Peaje.Ctras'First+Peaje.Cont-1);

   -- Mas casos en Lab09_Escenarios

   ---------------------
   -- Simplificar (2) --
   ---------------------
   new_line(3);
   Put_Line("***** Simplificar (2) *****");
   new_line;

   -- Caso 1: Se toman el primero, alguno/s del medio y el ultimo
   -- Resultado en S2_LP1_Res
   LP:= S2_LP1;
   Put_Line("La lista ((1.1,1.1),(2.2, 2.2),...) respetando un valor si y 3 no (con los extremos incluidos): ");
   Put_Line("Resultado correcto: ((1.1,1.1),(5.5,5.5),(9.9,9.9))");
   Simplificar(LP);
   new_line;
   mostrar_puntos(LP);
   new_line;
   LP:= S2_LP1_Res;
   Mostrar_Puntos(LP);

   new_line(3);

   -- Caso 2: Se toman el primero, alguno/s del medio y el ultimo
   -- Resultado en S2_LP2_Res
   LP:= S2_LP2;
   Put_Line("La lista ((0.1,0.1),(0.2, 0.2),...) respetando un valor si y 3 no (con los extremos incluidos): ");
   Put_Line("Resultado correcto: ((0.1,0.1),(0.5,0.5),...)");
   Simplificar(LP);
   new_line;
   mostrar_puntos(LP);
   new_line;
   LP:= S2_LP2_Res;
   Mostrar_Puntos(LP);

   new_line(3);

   -- Caso 3: Solo se toma el primero/ultimo valor (lista de 1 valor)
   -- Resultado en S2_LP3_Res
   LP:= S2_LP3;
   Put_Line("La lista ((0.1,0.1)) respetando un valor si y 3 no (con los extremos incluidos): ");
   Put_Line("Resultado correcto: ((0.1,0.1))");
   Simplificar(LP);
   new_line;
   Mostrar_Puntos(LP);
   new_line;
   LP:= S2_LP3_Res;
   Mostrar_Puntos(LP);

   new_line(3);

   -- Caso 4: Solo se toman el primer y el ultimo valor (lista de 2 valores)
   -- Resultado en S2_LP3_Res
   LP:= S2_LP4;
   Put_Line("La lista ((0.1,0.1),(0.2,0.2)) respetando un valor si y 3 no (con los extremos incluidos): ");
   Put_Line("Resultado correcto: ((0.1,0.1),(0.2,0.2))");
   Simplificar(LP);
   new_line;
   Mostrar_Puntos(LP);
   new_line;
   LP:= S2_LP4_Res;
   Mostrar_Puntos(LP);

   new_line(3);

   ---------------------
   -- Simplificar (3) --
   ---------------------
   new_line(3);
   Put_Line("***** Simplificar (3) *****");
   new_line;

   Put("(Descomentar las calls a mostrar_puntos_por_carretera para ver los objetos T_Estatica_Carreteras.Ctras.Trazado utilizados)");

   new_line(2);

   -- Caso 1: Se borran varios valores
   -- Resultado en S3_LC1_Res
   LC := S3_LC1;

   -- Descomentar para ver situacion inicial de las carreteras
   mostrar_puntos_por_carretera(LC);

   put_line("-------------");

   Simplificar(LC);
   mostrar_puntos_por_carretera(LC);

   put_line("-------------");

   LC := S3_LC1_Res;
   mostrar_puntos_por_carretera(LC);

   put_line("-------------------------");

   new_line(2);

   -- Caso 2: Se borran todos los valores excepto el primero y el ultimo de cada carretera
   -- Resultado en S3_LC2_Res
   LC := S3_LC2;

   -- Descomentar para ver situacion inicial de las carreteras
   mostrar_puntos_por_carretera(LC);

   put_line("-------------");

   Simplificar(LC);
   Mostrar_puntos_por_carretera(LC);

   put_line("-------------");

   LC := S3_LC2_Res;
   mostrar_puntos_por_carretera(LC);

   put_line("-------------------------");

   new_line(2);

   LC := S3_LC3;

   mostrar_puntos_por_carretera(LC);

   put_line("-------------");

   Simplificar(LC);
   Mostrar_puntos_por_carretera(LC);

   put_line("-------------");

   LC := S3_LC3_Res;
   mostrar_puntos_por_carretera(LC);

   put_line("-------------------------");

   new_line(2);


   --------------------------
   -- DNI_Mayor_Patrimonio --
   --------------------------
   new_line(3);
   Put_Line("***** DNI_Mayor_Patrimonio *****");
   new_line;

   Put_Line("(Descomentar las calls a mostrar_urbanizacion() para ver los objetos T_Urbanizacion utilizados)");
   new_line;

   -- Caso 1
   --Caso 1: Una urbanizaci�n. U1--> U1_Res
   U:= U1;

   -- (Descomentar para ver toda la informacion del objeto T_Urbanizacion)
   -- new_line;
   -- mostrar_urbanizacion(U);
   new_line;

   Put_Line("DNI del propietario con mayor patrimonio de la urbanizacion: 29334199 (Joseba Macazaga)" );
   put(DNI_Mayor_Patrimonio(U));


   new_line;


   -- Caso 2
   -- Caso 2: Una urbanizaci�n. U2--> U2_Res
   U:= U2;

   -- (Descomentar para ver toda la informacion del objeto T_Urbanizacion)
   -- new_line;
   -- mostrar_urbanizacion(U);
   new_line;

   Put_Line("DNI del propietario con mayor patrimonio de la urbanizacion: 62446217 (Carlos Amuchastegui)" );
   put(DNI_Mayor_Patrimonio(U));


   ------------------------------
   -- encontrar_espacio_blanco --
   ------------------------------
   new_line(3);
   Put_Line("***** encontrar_espacio_blanco *****");
   new_line;

   -- Caso 1 - Espacio en blanco en el medio
   -- Resultado: Puzle5_Res_F y Puzle5_Res_C
   P := Puzle5;
   Put_Line("Sudoku:");
   Put_Line("3 2 5 6 8 1 9 7 4");
   Put_Line("6 7 4 3 9 5 2 1 8");
   Put_Line("1 8 9 2 4 7 6 3 5");
   Put_Line("5 4 8 0 0 0 7 2 1");
   Put_Line("7 6 1 0 0 0 8 9 3");
   Put_Line("2 9 3 0 0 0 5 4 6");
   Put_Line("8 3 2 4 6 9 1 5 7");
   Put_Line("9 1 7 8 5 3 4 6 2");
   Put_Line("4 5 6 7 1 2 3 8 9");
   new_line;
   Put_Line("Coordenada superior izquierda donde se encuentra el espacio en blanco: (4,4)");
   encontrar_espacio_blanco(P,FF,CC);
   Put_Line(FF'img & "," & CC'img);

   new_line(2);

   -- Caso 2 - Espacio en blanco al principio
   -- Resultado: Puzle1_Res_F y Puzle1_Res_C
   P := Puzle1;
   Put_Line("Sudoku:");
   Put_Line("0 0 0 2 1 5 9 4 8");
   Put_Line("0 0 0 3 9 7 6 2 1");
   Put_Line("0 0 0 8 4 6 5 7 3");
   Put_Line("1 3 2 7 6 4 8 9 5");
   Put_Line("5 8 9 1 2 3 7 6 4");
   Put_Line("6 4 7 9 5 8 3 1 2");
   Put_Line("9 2 5 6 3 1 4 8 7");
   Put_Line("7 6 4 5 8 2 1 3 9");
   Put_Line("8 1 3 4 7 9 2 5 6");
   new_line;
   Put_Line("Coordenada superior izquierda donde se encuentra el espacio en blanco: (1,1)");
   encontrar_espacio_blanco(P,FF,CC);
   Put_Line(FF'img & "," & CC'img);

   new_line(2);

   -- Caso 3 - Espacio en blanco al final
   -- Resultado: Puzle9_Res_F y Puzle9_Res_C
   P := Puzle9;
   Put_Line("Sudoku:");
   Put_Line("5 9 7 4 1 8 3 2 6");
   Put_Line("6 3 1 7 5 2 9 4 8");
   Put_Line("4 8 2 6 3 9 1 7 5");
   Put_Line("8 1 9 3 4 7 6 5 2");
   Put_Line("3 6 4 2 9 5 8 1 7");
   Put_Line("2 7 5 8 6 1 4 3 9");
   Put_Line("7 2 6 1 8 3 0 0 0");
   Put_Line("9 4 3 5 2 6 0 0 0");
   Put_Line("1 5 8 9 7 4 0 0 0");
   new_line;
   Put_Line("Coordenada superior izquierda donde se encuentra el espacio en blanco: (7,7)");
   encontrar_espacio_blanco(P,FF,CC);
   Put_Line(FF'img & "," & CC'img);

   new_line(2);

   -- Caso 4 - Espacio en blanco en el 4.1
   -- Resultado: Puzle9_Res_F y Puzle9_Res_C
   P := Puzle4;
   Put_Line("Coordenada superior izquierda donde se encuentra el espacio en blanco: (4,1)");
   encontrar_espacio_blanco(P,FF,CC);
   Put_Line(FF'img & "," & CC'img);

   --Mas casos en Lab09_Escenarios

   -----------------------------
   -- rotar_matriz_derecha_90 --
   -----------------------------
   new_line(3);
   Put_Line("***** rotar_matriz_derecha_90 *****");
   new_line;

   -- Caso 1
   -- Resultado: Ficha2
   ficha:= Ficha1;
   Put_Line("La ficha: ");
   Put_Line("1 2 3");
   Put_Line("4 5 6");
   Put_Line("7 8 9");
   Put_Line("Rotada 90 grados a la derecha quedaria:");
   Put_Line("7 4 1");
   Put_Line("8 5 2");
   Put_Line("9 6 3");
   rotar_matriz_derecha_90(ficha);
   Put_Line("Resultado: ");
   mostrar_ficha(ficha);

   new_line(2);

   -- Caso 2
   ficha:= (
      (1,1,1),
      (2,2,2),
      (3,3,3)
   );
   Put_Line("La ficha: ");
   Put_Line("1 1 1");
   Put_Line("2 2 2");
   Put_Line("3 3 3");
   Put_Line("Rotada 90 grados a la derecha quedaria:");
   Put_Line("3 2 1");
   Put_Line("3 2 1");
   Put_Line("3 2 1");
   rotar_matriz_derecha_90(ficha);
   Put_Line("Resultado: ");
   mostrar_ficha(ficha);

   new_line(2);

   --Mas casos en Lab09_Escenarios

   ---------------------
   -- filas_correctas --
   ---------------------
   new_line(3);
   Put_Line("***** filas_correctas *****");
   new_line;

   -- Caso 1: Insercion en el primer panel
   -- Resultado Puzle10_Res
   P:= Puzle10;
   ficha := Ficha10;
   Put_Line("Se puede implementar la ficha");
   new_line;
   Put_Line("4 9 8");
   Put_Line("7 5 3");
   Put_Line("6 1 2");
   new_line;
   Put_Line("En el sudoku");
   new_line;
   Put_Line("0 0 0 1 2 6 7 3 5");
   Put_Line("0 0 0 4 9 8 6 2 1");
   Put_Line("0 0 0 7 3 5 8 9 4");
   Put_Line("3 8 9 6 1 2 4 5 7");
   Put_Line("5 4 1 3 7 9 2 6 8");
   Put_Line("2 6 7 5 8 4 3 1 9");
   Put_Line("9 2 4 8 6 1 5 7 3");
   Put_Line("1 7 5 2 4 3 9 8 6");
   Put_Line("8 3 6 9 5 7 1 4 2 ? TRUE");
   Put_Line(filas_correctas(P, ficha, 1, 1)'img);

   new_line(2);

   -- Caso 2: Insercion en el panel del medio
   -- Resultado en Puzle11_Res

   P:= Puzle11;
   ficha := Ficha11;

   Put_Line("Se puede implementar la ficha");
   new_line;
   Put_Line("1 2 3");
   Put_Line("4 5 6");
   Put_Line("7 8 9");
   new_line;
   Put_Line("En el sudoku");
   new_line;
   Put_Line("6 7 3 5 9 4 2 1 8");
   Put_Line("4 9 2 8 1 6 7 5 3");
   Put_Line("5 1 8 3 7 2 6 4 9");
   Put_Line("3 6 7 0 0 0 4 2 1");
   Put_Line("9 2 1 0 0 0 8 3 5");
   Put_Line("8 5 4 0 0 0 9 7 6");
   Put_Line("1 4 6 2 3 9 5 8 7");
   Put_Line("2 8 5 7 6 1 3 9 4");
   Put_Line("7 3 9 4 5 8 1 6 2 ? FALSE");
   Put_Line(filas_correctas(P, ficha, 4, 4)'img);

   new_line(2);

   -- Caso 3: Insercion en el panel del final
   -- Resultado: Puzle12_Res
   P:= Puzle12;
   ficha := Ficha12;

   Put_Line("Se puede implementar la ficha");
   new_line;
   Put_Line("7 3 5");
   Put_Line("1 4 8");
   Put_Line("6 9 2");
   new_line;
   Put_Line("En el sudoku");
   new_line;
   Put_Line("7 9 5 4 1 2 3 8 6");
   Put_Line("8 4 3 5 6 9 2 1 7");
   Put_Line("6 1 2 8 7 3 4 5 9");
   Put_Line("2 3 1 7 9 5 8 6 4");
   Put_Line("9 7 4 1 8 6 5 2 3");
   Put_Line("5 6 8 2 3 4 9 7 1");
   Put_Line("1 2 6 9 4 8 0 0 0");
   Put_Line("3 5 9 6 2 7 0 0 0");
   Put_Line("4 8 7 3 5 1 0 0 0 ? TRUE");
   Put_Line(Filas_Correctas(P, Ficha, 7, 7)'Img);

end Lab09_pruebas;