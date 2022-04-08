Взаимодействие с языком C
=========================

.. include:: ../../global.txt

Ada позволяет нам взаимодействовать с кодом на многих языках, включая
C и C++. В этом разделе обсуждается, как взаимодействовать с C.

Многоязычный проект
-------------------

По умолчанию при использовании :program:`gprbuild` мы компилируем только
исходные файлы Ada. Чтобы скомпилировать файлы C, нам нужно изменить файл
проекта, используемый :program:`gprbuild`. Мы используем запись
``Languages``, как в следующем примере:

.. code-block:: ada

    project Multilang is

       for Languages use ("ada", "c");

       for Source_Dirs use ("src");
       for Main use ("main.adb");
       for Object_Dir use "obj";

    end Multilang;

Соглашение о типах
------------------

Для взаимодействия с типами данных, объявленными в приложении C,
необходимо указать аспект :ada:`Convention` в соответствующем объявлении
типа Ada. В следующем примере выполняется интерфейс с перечислением
:ada:`C_Enum`, объявленным в исходном файле C:

.. code-block:: ada

    procedure Show_C_Enum is

       type C_Enum is (A, B, C)
         with Convention => C;
       --  Use C convention for C_Enum
    begin
       null;
    end Show_C_Enum;

Для взаимодействия со встроенными типами C используется пакет
:ada:`Interfaces.C`, содержащий большинство необходимых определений
типов. Например:

.. code-block:: ada

    with Interfaces.C; use Interfaces.C;

    procedure Show_C_Struct is

       type c_struct is record
          a : int;
          b : long;
          c : unsigned;
          d : double;
       end record
         with Convention => C;

    begin
       null;
    end Show_C_Struct;

Здесь мы взаимодействуем со структурой C (:ada:`C_Struct`) и используем
соответствующие типы данных в C (:c:`int`, :c:`long`, :c:`unsigned` и
:c:`double`). Это объявление в C:

.. code-block:: c

    !c_struct.h
    struct c_struct
    {
        int         a;
        long        b;
        unsigned    c;
        double      d;
    };

Подпрограммы на других языках
-----------------------------

Вызов подпрограмм C в Ada
~~~~~~~~~~~~~~~~~~~~~~~~~

Мы используем аналогичный подход при взаимодействии с подпрограммами,
написанными на C. Рассмотрим следующее объявление в заголовочном файле
C:

.. code-block:: c

    !my_func.h
    int my_func (int a);

Вот соответствующее определение функции на C:

.. code-block:: c

    !my_func.c
    #include "my_func.h"

    int my_func (int a)
    {
        return a * 2;
    }

Мы можем связать этот код с кодом на Аде, используя аспект :ada:`Import`.
Например:

.. code-block:: ada

    with Interfaces.C; use Interfaces.C;
    with Ada.Text_IO;  use Ada.Text_IO;

    procedure Show_C_Func is

       function my_func (a : int) return int
         with
           Import        => True,
           Convention    => C;

       --  Imports function 'my_func' from C.
       --  You can now call it from Ada.

       V : int;
    begin
       V := my_func (2);
       Put_Line ("Result is " & int'Image (V));
    end Show_C_Func;

При необходимости можно использовать другое имя подпрограммы в коде
Ada. Например, можно вызвать функцию C :ada:`Get_Value`:

.. code-block:: ada

    with Interfaces.C; use Interfaces.C;
    with Ada.Text_IO;  use Ada.Text_IO;

    procedure Show_C_Func is

       function Get_Value (a : int) return int
         with
           Import        => True,
           Convention    => C,
           External_Name => "my_func";

       --  Imports function 'my_func' from C and
       --  rename it to 'Get_Value'

       V : int;
    begin
       V := Get_Value (2);
       Put_Line ("Result is " & int'Image (V));
    end Show_C_Func;

Вызов подпрограмм Ada в C
~~~~~~~~~~~~~~~~~~~~~~~~~

Вы также можете вызывать подпрограммы Ada из приложений C. Вы делаете
это с аспектом :ada:`Export`. Например:

.. code-block:: ada

    with Interfaces.C; use Interfaces.C;

    package C_API is

       function My_Func (a : int) return int
         with
           Export        => True,
           Convention    => C,
           External_Name => "my_func";

    end C_API;

Это соответствующее тело подпрограммы, реализующий эту функцию:

.. code-block:: ada

    package body C_API is

       function My_Func (a : int) return int is
       begin
          return a * 2;
       end My_Func;

    end C_API;

На стороне C мы делаем то же самое, что и если бы функция была
написана на C: просто объявляем ее с помощью ключевого слова :c:`extern`.
Например:

.. code-block:: c

    !main.c
    #include <stdio.h>

    extern int my_func (int a);

    int main (int argc, char **argv) {

      int v = my_func(2);

      printf("Result is %d\n", v);

      return 0;
    }

Внешние переменные
------------------

Использование глобальных переменных C в Ada
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Чтобы использовать глобальные переменные из кода C, мы используем тот
же метод, что и подпрограммы: мы указываем аспекты :ada:`Import` и
:ada:`Convention` для каждой переменной, которую мы хотим импортировать.

Давайте повторно воспользуемся примером из предыдущего раздела. Мы
добавим глобальную переменную (:c:`func_cnt`) для подсчета количества вызовов
функции (:c:`my_func`):

.. code-block:: c

    !test.h
    extern int func_cnt;

    int my_func (int a);

Переменная объявлена в файле C и увеличивается в :c:`my_func`:

.. code-block:: c

    !test.c
    #include "test.h"

    int func_cnt = 0;

    int my_func (int a)
    {
      func_cnt++;

      return a * 2;
    }

В приложении Ada мы просто ссылаемся на внешнюю переменную:

.. code-block:: ada

    with Interfaces.C; use Interfaces.C;
    with Ada.Text_IO;  use Ada.Text_IO;

    procedure Show_C_Func is

       function my_func (a : int) return int
         with
           Import     => True,
           Convention => C;

       V : int;

       func_cnt : int
         with
           Import        => True,
           Convention    => C;
       --  We can access the func_cnt variable
       --  from test.c

    begin
       V := my_func (1);
       V := my_func (2);
       V := my_func (3);
       Put_Line ("Result is " & int'Image (V));

       Put_Line ("Function was called "
                 & int'Image (func_cnt)
                 & " times");
    end Show_C_Func;

Как мы видим, запустив приложение, значение счетчика - это количество
вызовов :c:`my_func`.

Мы можем использовать аспект :ada:`External_Name`, чтобы дать другое имя
переменной в приложении Ada, как мы это делаем для подпрограмм.

Использование переменных Ada в C
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Вы также можете использовать переменные, объявленные в файлах Ada, в
приложениях C. Точно так же, как мы делали для подпрограмм, вы делаете
это с аспектом :ada:`Export`.

Давайте повторно воспользуемся прошлым примером и добавим счетчик, как
в предыдущем примере, но на этот раз увеличим счетчик в коде Ada:

.. code-block:: ada

    with Interfaces.C; use Interfaces.C;

    package C_API is

       func_cnt : int := 0
         with
           Export     => True,
           Convention => C;

       function My_Func (a : int) return int
         with
           Export        => True,
           Convention    => C,
           External_Name => "my_func";

    end C_API;

Затем переменная увеличивается в :ada:`My_Func`:

.. code-block:: ada

    package body C_API is

       function My_Func (a : int) return int is
       begin
          func_cnt := func_cnt + 1;
          return a * 2;
       end My_Func;

    end C_API;

В приложении C нам просто нужно объявить переменную и использовать ее:

.. code-block:: c

    !main.c
    #include <stdio.h>

    extern int my_func (int a);

    extern int func_cnt;

    int main (int argc, char **argv) {

      int v;

      v = my_func(1);
      v = my_func(2);
      v = my_func(3);

      printf("Result is %d\n", v);

      printf("Function was called %d times\n", func_cnt);

      return 0;
    }

Опять же, запустив приложение, мы видим, что значение счетчика ‑ это
количество вызовов :c:`my_func`.

Создание привязок
-----------------

В приведенных выше примерах мы вручную добавили аспекты в наш код Ada,
чтобы они соответствовали исходному коду C, с которым мы
взаимодействуем. Это называется созданием *привязки*. Мы можем
автоматизировать этот процесс, используя параметр компилятора
*дампа спецификации Ada*: ``-fdump-ada-spec``. Мы проиллюстрируем
это, вернувшись к нашему предыдущему примеру.

Это был наш заголовочный файл C:

.. code-block:: c

    !my_func.c
    extern int func_cnt;

    int my_func (int a);

Чтобы создать привязку Ada, мы вызовем компилятор следующим образом:

.. code-block:: sh

    gcc -c -fdump-ada-spec -C ./test.h

Результатом является файл спецификации Ada с именем :file:`test_h.ads`:

.. code-block:: ada

    pragma Ada_2005;
    pragma Style_Checks (Off);

    with Interfaces.C; use Interfaces.C;

    package test_h is

       func_cnt : aliased int;  -- ./test.h:3
       pragma Import (C, func_cnt, "func_cnt");

       function my_func (arg1 : int) return int;  -- ./test.h:5
       pragma Import (C, my_func, "my_func");

    end test_h;

Теперь мы просто ссылаемся на этот пакет :file:`test_h` в нашем приложении Ada:

.. code-block:: ada

    with Interfaces.C; use Interfaces.C;
    with Ada.Text_IO;  use Ada.Text_IO;
    with test_h;       use test_h;

    procedure Show_C_Func is
       V : int;
    begin
       V := my_func (1);
       V := my_func (2);
       V := my_func (3);
       Put_Line ("Result is " & int'Image (V));

       Put_Line ("Function was called "
                 & int'Image (func_cnt)
                 & " times");
    end Show_C_Func;

Вы можете указать имя родительского модуля для создаваемых привязок в
качестве операнда для ``fdump-ada-spec``:

.. code-block:: sh

    gcc -c -fdump-ada-spec -fada-spec-parent=Ext_C_Code -C ./test.h

Это создает файл :file:`ext_c_code-test_h.ads`:

.. code-block:: ada
    :class: ada-syntax-only

    package Ext_C_Code.test_h is

       --  automatic generated bindings...

    end Ext_C_Code.test_h;

Адаптация привязок
~~~~~~~~~~~~~~~~~~

Компилятор делает все возможное при создании привязок для файла
заголовка C. Однако иногда приходится догадываться о переводе, и
сгенерированные привязки не всегда соответствуют нашим ожиданиям.
Например, это может произойти при создании привязок для функций,
которые имеют указатели в качестве аргументов. В этом случае
компилятор может использовать :ada:`System.Address` как тип одного или нескольких
указателей. Хотя этот подход работает нормально (как мы увидим позже),
обычно человек не так интерпретирует заголовочный файл C. Следующий
пример иллюстрирует эту проблему.

Начнем с этого заголовочного файла C:

.. code-block:: c

    !test.h
    struct test;

    struct test * test_create(void);

    void test_destroy(struct test *t);

    void test_reset(struct test *t);

    void test_set_name(struct test *t, char *name);

    void test_set_address(struct test *t, char *address);

    void test_display(const struct test *t);

И соответствующая реализация C:

.. code-block:: c

    !test.c
    #include <stdlib.h>
    #include <string.h>
    #include <stdio.h>

    #include "test.h"

    struct test {
      char name[80];
      char address[120];
    };

    static size_t
    strlcpy(char *dst, const char *src, size_t dstsize)
    {
      size_t len = strlen(src);
      if (dstsize) {
        size_t bl = (len < dstsize-1 ? len : dstsize-1);
        ((char*)memcpy(dst, src, bl))[bl] = 0;
      }
      return len;
    }

    struct test * test_create(void)
    {
      return malloc (sizeof (struct test));
    }

    void test_destroy(struct test *t)
    {
      if (t != NULL) {
        free(t);
      }
    }

    void test_reset(struct test *t)
    {
      t->name[0]    = '\0';
      t->address[0] = '\0';
    }

    void test_set_name(struct test *t, char *name)
    {
      strlcpy(t->name, name, sizeof(t->name));
    }

    void test_set_address(struct test *t, char *address)
    {
      strlcpy(t->address, address, sizeof(t->address));
    }

    void test_display(const struct test *t)
    {
      printf("Name:    %s\n", t->name);
      printf("Address: %s\n", t->address);
    }

Далее мы создадим наши привязки:

.. code-block:: sh

    gcc -c -fdump-ada-spec -C ./test.h

Это создает следующую спецификацию в :file:`test_h.ads`:

.. code-block:: ada

    pragma Ada_2005;
    pragma Style_Checks (Off);

    with Interfaces.C; use Interfaces.C;
    with System;
    with Interfaces.C.Strings;

    package test_h is

       --  skipped empty struct test

       function test_create return System.Address;  -- ./test.h:5
       pragma Import (C, test_create, "test_create");

       procedure test_destroy (arg1 : System.Address);  -- ./test.h:7
       pragma Import (C, test_destroy, "test_destroy");

       procedure test_reset (arg1 : System.Address);  -- ./test.h:9
       pragma Import (C, test_reset, "test_reset");

       procedure test_set_name (arg1 : System.Address; arg2 : Interfaces.C.Strings.chars_ptr);  -- ./test.h:11
       pragma Import (C, test_set_name, "test_set_name");

       procedure test_set_address (arg1 : System.Address; arg2 : Interfaces.C.Strings.chars_ptr);  -- ./test.h:13
       pragma Import (C, test_set_address, "test_set_address");

       procedure test_display (arg1 : System.Address);  -- ./test.h:15
       pragma Import (C, test_display, "test_display");

    end test_h;

Как мы видим, генератор привязки полностью игнорирует объявление :c:`struct test`, и
все ссылки на структуру :c:`test` заменяются адресами (:ada:`System.Address`). Тем не менее, эти
привязки достаточно хороши, чтобы позволить нам создать тестовое
приложение на Ada:

.. code-block:: ada

    with Interfaces.C;         use Interfaces.C;
    with Interfaces.C.Strings; use Interfaces.C.Strings;
    with Ada.Text_IO;          use Ada.Text_IO;
    with test_h;               use test_h;

    with System;

    procedure Show_Automatic_C_Struct_Bindings is

       Name    : constant chars_ptr :=
         New_String ("John Doe");
       Address : constant chars_ptr :=
         New_String ("Small Town");

       T : System.Address := test_create;

    begin
       test_reset (T);
       test_set_name (T, Name);
       test_set_address (T, Address);

       test_display (T);
       test_destroy (T);
    end Show_Automatic_C_Struct_Bindings;

Мы можем успешно связать наш код C с Ada, используя автоматически
сгенерированные привязки, но они не идеальны. Вместо этого мы
предпочли бы привязки Ada, которые соответствуют нашей (человеческой)
интерпретации файла заголовка C. Это требует ручного анализа файла
заголовка. Хорошая новость заключается в том, что мы можем
использовать автоматически сгенерированные привязки в качестве
отправной точки и адаптировать их к нашим потребностям. Например, мы
можем:

#. Определите тип :ada:`Test` на основе :ada:`System.Address` и используйте его во всех соответствующих
   функциях.

#. Удалите префикс ``test_`` во всех операциях с типом :ada:`Test`.

Вот итоговая спецификация:

.. code-block:: ada

    with Interfaces.C; use Interfaces.C;
    with System;
    with Interfaces.C.Strings;

    package adapted_test_h is

       type Test is new System.Address;

       function Create return Test;
       pragma Import (C, Create, "test_create");

       procedure Destroy (T : Test);
       pragma Import (C, Destroy, "test_destroy");

       procedure Reset (T : Test);
       pragma Import (C, Reset, "test_reset");

       procedure Set_Name (T    : Test;
                           Name : Interfaces.C.Strings.chars_ptr);  -- ./test.h:11
       pragma Import (C, Set_Name, "test_set_name");

       procedure Set_Address (T       : Test;
                              Address : Interfaces.C.Strings.chars_ptr);
       pragma Import (C, Set_Address, "test_set_address");

       procedure Display (T : Test);  -- ./test.h:15
       pragma Import (C, Display, "test_display");

    end adapted_test_h;

И это соответствующее тело Ada:

.. code-block:: ada

    with Interfaces.C;         use Interfaces.C;
    with Interfaces.C.Strings; use Interfaces.C.Strings;
    with adapted_test_h;       use  adapted_test_h;

    with System;

    procedure Show_Adapted_C_Struct_Bindings is

       Name    : constant chars_ptr :=
         New_String ("John Doe");
       Address : constant chars_ptr :=
         New_String ("Small Town");

       T : Test := Create;

    begin
       Reset (T);
       Set_Name (T, Name);
       Set_Address (T, Address);

       Display (T);
       Destroy (T);
    end Show_Adapted_C_Struct_Bindings;

Теперь мы можем использовать тип :ada:`Test` и его операции в понятной и
удобочитаемой форме.


