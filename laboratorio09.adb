package body Laboratorio09 is

   ------------------------------------------------------------
   --AUTHOR: BERGARETXE LOPEZ, LUKA
   ------------------------------------------------------------

   -----------------------
   -- Insertar_En_Medio --
   -----------------------

   procedure Insertar_En_Medio (L: in out T_Lista_Estatica; Num: in Integer)
   is
   begin
      if L.cont = 0 then
         L.Dato(L.Dato'first) := Num;
         L.cont := 1;
      elsif L.cont mod 2 = 0 and L.cont /= Max_Valores then
         L.Dato(L.Dato'first..L.Dato'first+L.cont) := L.Dato(L.Dato'first..L.Dato'first+L.cont/2-1)
            & Num & L.Dato(L.Dato'first+L.cont/2..L.Dato'first+L.cont-1);
         L.cont := L.cont + 1;
      end if;

   end Insertar_En_Medio;

   -----------------------
   -- Borrar_Intermedio --
   -----------------------

   procedure Borrar_Intermedio (L: in out T_Lista_Estatica) is
   begin
      if L.cont mod 2 /= 0 then
         L.Dato(L.Dato'first..L.Dato'first+L.Cont-2) := L.Dato(L.Dato'first..L.Dato'first+L.cont/2-1)
            & L.Dato(L.Dato'first+L.cont/2+1..L.Dato'first+L.cont-1);
         L.cont := L.cont - 1;
      end if;

   end Borrar_Intermedio;

   ---------------
   -- Encriptar --
   ---------------

   procedure Encriptar
     (N,Clave: in T_Vector_Enteros;
      Num: out T_Vector_Enteros)
   is
   begin
      Num := Clave;
      for i in Clave'range loop
         if i > Clave'Length - N'Length then
            Num(Clave(i)) := N(i - (Clave'Length - N'Length));
         else
            Num(Clave(i)) := 0;
         end if;
      end loop;

   end Encriptar;

   -------------
   -- Comunes --
   -------------

   function Comunes (V1,V2: in T_vector_enteros) return Natural is
      i, j : Natural := 0;
      r : Natural := 0;
   begin
      if V1'Length > 0 and V2'Length > 0 then
         while i < V1'Length and j < V2'Length loop
            if V1(V1'first + i) < V2(V2'first + j) then
               i := i + 1;
            elsif V1(V1'first + i) > V2(V2'first + j) then
               j := j + 1;
            else
               r := r + 1;
               i := i + 1;
               j := j + 1;
            end if;
         end loop;
      end if;
      return r;
   end Comunes;

   --------------------------------------
   -- Obtener_Num_Vecinos_por_Vivienda --
   --------------------------------------

   procedure Obtener_Num_Vecinos_por_Vivienda
     (C: in T_Comunidad;
      R:out T_Rascacielos)
   is
   begin
      R := (others =>(others =>0));
      for i in C'range loop
         R(C(i).piso, C(i).mano) := R(C(i).piso, C(i).mano) + 1;
      end loop;

   end Obtener_Num_Vecinos_por_Vivienda;

   ----------------------
   -- Obtener_Consumos --
   ----------------------

   procedure Obtener_Consumos
     (E: in T_Edificio;
      Consumo_electrico: out Float;
      Consumo_Gas: out T_Consumo_Medio_Manos)
   is
      hab_mano, hab_edif : Natural := 0;
   begin
      Consumo_electrico := 0.0;
      Consumo_Gas := (others => 0.0);
      for i in E'range(1) loop
         hab_mano := 0;
         for j in E'range(2) loop
            hab_mano := hab_mano + E(i,j).Habitantes;
            Consumo_electrico := Consumo_electrico + E(i,j).Consumo_Electrico;
            Consumo_Gas(i) := Consumo_Gas(i) + E(i,j).Consumo_Gas;
         end loop;
         Consumo_Gas(i) := Consumo_Gas(i) / float(hab_mano);
         hab_edif := hab_edif + hab_mano;

      end loop;
      Consumo_electrico := Consumo_electrico / float(hab_edif);

   end Obtener_Consumos;

   ------------
   -- Maximo --
   ------------

   procedure Maximo
     (Lluvia: in T_Datos_Pluviometricos;
      F1, F2: T_Fecha;
      F: out T_Fecha)
   is
      F3 : T_Fecha := F1;
      F4 : T_Fecha := (31, Diciembre, F2.anno);
      l1, l2 : Natural := 0;
   begin
      F.Dia := 1;
      if F1.anno = F2.anno and F1.Mes = F2.Mes then
         F.Mes := F1.Mes;
         F.anno := F1.anno;
      else
         for anno in F3.anno..F4.anno loop
            if anno = F2.anno then
               F4.Mes := F2.Mes;
            end if;

            for mes in F3.Mes..F4.Mes loop
               if anno = F2.anno and mes = F2.Mes then
                  F4.Dia := F2.Dia;
               elsif mes = Febrero then
                  if anno mod 400 = 0 or (anno mod 100 = 0 xor anno mod 4 = 0) then
                     F4.Dia := 29;
                  else
                     F4.Dia := 28;
                  end if;
               elsif (T_Mes'Pos(mes) < 7 and T_Mes'Pos(mes) mod 2 = 0) or (T_Mes'Pos(mes) >= 7 and T_Mes'Pos(mes) mod 2 = 1) then
                  F4.Dia := 31;
               else
                  F4.Dia := 30;
               end if;

               for dia in F3.Dia..F4.Dia loop
                  l2 := l2 + Lluvia(anno)(mes)(dia);
               end loop;
               if l2 > l1 then
                  F.Mes := mes;
                  F.anno := anno;
                  l1 := l2;
               end if;
               l2 := 0;
               F3.Dia := 1;
            end loop;
            F3.Mes := Enero;
         end loop;
      end if;
   end Maximo;

   -----------------
   -- Simplificar --
   -----------------

   procedure Simplificar
     (L:  in out T_Estatica_Carreteras;
      Peaje: out T_Estatica_Carreteras)
   is
      i : Natural := 1;
   begin
      Peaje.Cont := 0;
      if L.Cont > 0 then
         loop
            if L.Ctras(i).Peaje_km > 0.0 then
               Peaje.Ctras (Peaje.Cont + 1) := L.Ctras(i);
               Peaje.Cont := Peaje.Cont + 1;
               L.Cont := L.Cont - 1;
            elsif Peaje.Cont > 0 then
               L.Ctras(i - Peaje.Cont) := L.Ctras(i);
            end if;
            i := i + 1;
            exit when Peaje.Cont = 5 or i > L.Cont + Peaje.Cont;
         end loop;
         if Peaje.Cont = 5 then
            L.Ctras(i - Peaje.Cont) := L.Ctras(i);
         end if;
      end if;
      null;
   end Simplificar;

   -----------------
   -- Simplificar --
   -----------------

   procedure Simplificar (L: in out T_Estatica_Puntos) is
      j : Natural := 2;
   begin
      if L.Num >= 2 then
         for i in 2..L.Num - 1 loop
            if i mod 4 = 1 then
               L.Ptos(j) := L.Ptos(i);
               j := j + 1;
            end if;
         end loop;
         L.Ptos(j) := L.Ptos(L.Num);
         L.Ptos(j + 1) := (-1.0,-1.0);
         L.Num := j;
      end if;
   end Simplificar;

   -----------------
   -- Simplificar --
   -----------------

   procedure Simplificar (L: in out T_Estatica_Carreteras) is
      j : Natural;
   begin
      for c in 1..L.Cont loop
         j := 2;
         for i in 2..L.Ctras(c).Trazado.Num - 1 loop
            if abs(L.Ctras(c).Trazado.Ptos(i).X - L.Ctras(c).Trazado.Ptos(i - 1).X) >= 0.001
            or abs(L.Ctras(c).Trazado.Ptos(i).Y - L.Ctras(c).Trazado.Ptos(i - 1).Y) >= 0.001 then
               L.Ctras(c).Trazado.Ptos(j) := L.Ctras(c).Trazado.Ptos(i);
               j := j + 1;
            end if;
         end loop;
         L.Ctras(c).Trazado.Ptos(j) := L.Ctras(c).Trazado.Ptos(L.Ctras(c).Trazado.Num);
         L.Ctras(c).Trazado.Num := j;
      end loop;
   end Simplificar;

   --------------------------
   -- DNI_Mayor_Patrimonio --
   --------------------------

   function DNI_Mayor_Patrimonio (U: in T_Urbanizacion) return Integer is
      r : Integer;
      Max_Propietarios : constant Integer := 20;
      type T_L_Propietarios is array (1..Max_Propietarios, 1..2) of Integer;
      l_prop : T_L_Propietarios := (others => (-1,0));
      l : Natural;
      patr_max : Integer := Integer'First;
   begin
      for i in U'range loop
         for j in U(i)'range loop
            for k in 1..U(i)(j).Num_Apartamentos loop
               l := 1;
               while l_prop(l,1) /= -1 and U(i)(j).Apartamentos(k).Propietario_Actual.Dni /= l_prop(l,1) loop
                  l := l + 1;
               end loop;
               l_prop(l,1) := U(i)(j).Apartamentos(k).Propietario_Actual.Dni;
               l_prop(l,2) := l_prop(l,2) + U(i)(j).Apartamentos(k).Precio;
            end loop;
         end loop;
      end loop;
      l := 1;
      while l_prop(l,1) /= -1 loop
         patr_max := (Boolean'Pos(l_prop(l,2) < patr_max)) * patr_max + (Boolean'Pos(l_prop(l,2) >= patr_max)) * l_prop(l,2);
         r := (Boolean'Pos(l_prop(l,2) < patr_max)) * r + (Boolean'Pos(l_prop(l,2) >= patr_max)) * l_prop(l,1);
         l := l + 1;
      end loop;
      return r;
   end DNI_Mayor_Patrimonio;

   ------------------------------
   -- encontrar_espacio_blanco --
   ------------------------------

   procedure encontrar_espacio_blanco (P: in T_Puzle; F, C: out Integer) is
      i, j : Positive := 1;
   begin
      while P(i, j) /= 0 loop
         i := i + j / N;
         j := j mod N + 1;
      end loop;
      F := 1 + i / M * M;
      C := 1 + j / M * M;
   end encontrar_espacio_blanco;

   -----------------------------
   -- rotar_matriz_derecha_90 --
   -----------------------------

   procedure rotar_matriz_derecha_90 (F: in out T_Ficha) is
      ax : Natural;
   begin
      for i in 1..F'length(2) / 2 loop
         for j in F'first(2)+i-1..F'last(2)-i loop
            ax := F(i,j);
            F(i,j) := F(F'last(1)-j+1,i);
            F(F'last(1)-j+1,i) := F(F'first(1)+j-1,F'last(2)-i+1);
            F(F'first(1)+j-1,F'last(2)-i+1) := ax;
            ax := F(F'last(1)-j+1,i);
            F(F'last(1)-j+1,i) := F(F'last(2)-i+1,F'last(1)-j+1);
            F(F'last(2)-i+1,F'last(1)-j+1) := ax;
         end loop;
      end loop;
   end rotar_matriz_derecha_90;

   ---------------------
   -- filas_correctas --
   ---------------------

   function filas_correctas
     (P: in T_Puzle;
      Ficha: in T_Ficha;
      F, C: in Integer)
      return Boolean
   is
      r : Boolean := True;
      i, j : Natural := 1;
      k, l : Natural := 0;
      v1, v2 : Integer;
   begin
      loop -- Comprobar ficha
         i := i + j / Ficha'Last(2);
         j := j mod Ficha'Last(2) + 1;
         k := i + j / Ficha'Last(2);
         l := j mod Ficha'Last(2) + 1;
         while r and k <= Ficha'Last(1) loop
            r := r and Ficha(k,l) /= Ficha(i,j);
            k := k + l / Ficha'Last(2);
            l := l mod Ficha'Last(2) + 1;
         end loop;
         exit when not r or i > Ficha'Last(1);
      end loop;

      if r then -- Comprobar filas
         i := 1;
         while r and i <= P'Last(1) loop
            j := 1;
            while r and j < P'Last(2) loop
               k := j + 1;
               if i in F..F+Ficha'Length(1)-1 and j in C..C+Ficha'Length(2)-1 then
                  v1 := Ficha(i-F+1,j-C+1);
               else
                  v1 := P(i,j);
               end if;
               while r and k <= P'Last(2) loop
                  if i in F..F+Ficha'Length(1)-1 and k in C..C+Ficha'Length(2)-1 then
                     v2 := Ficha(i-F+1,k-C+1);
                  else
                     v2 := P(i,k);
                  end if;
                  r := r and v1 /= v2;
                  k := k + 1;
               end loop;
               j := j + 1;
            end loop;
            i := i + 1;
         end loop;
      end if;

      if r then -- Comprobar columnas
         j := 1;
         while r and j <= P'Last(2) loop
            i := 1;
            while r and i < P'Last(1) loop
               k := i + 1;
               if i in F..F+Ficha'Length(1)-1 and j in C..C+Ficha'Length(2)-1 then
                  v1 := Ficha(i-F+1,j-C+1);
               else
                  v1 := P(i,j);
               end if;
               while r and k <= P'Last(2) loop
                  if k in F..F+Ficha'Length(1)-1 and j in C..C+Ficha'Length(2)-1 then
                     v2 := Ficha(k-F+1,j-C+1);
                  else
                     v2 := P(k,j);
                  end if;
                  r := r and v1 /= v2;
                  k := k + 1;
               end loop;
               i := i + 1;
            end loop;
            j := j + 1;
         end loop;
      end if;

      return r;

   end filas_correctas;

end Laboratorio09;
