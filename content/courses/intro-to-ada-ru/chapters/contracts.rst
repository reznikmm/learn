Контрактное проектирование
==========================

.. include:: ../../global.txt

Контракты используются в программировании для кодификации ожиданий.
Режимы параметров подпрограммы можно рассматривать как простую форму
контрактов. Когда спецификация подпрограммы :ada:`Op` объявляет параметр,
использующий режим :ada:`in`, вызывающий :ada:`Op` знает, что аргумент
:ada:`in` не будет изменен :ada:`Op`. Другими словами, вызывающий ожидает,
что :ada:`Op` не изменяет предоставляемый им аргумент, а просто считывает
информацию, хранящуюся в аргументе. Ограничения и подтипы являются другими
примерами контрактов. В целом, эти спецификации улучшают согласованность
приложения.

Программирование по контракту (*Design-by-contract*) относится к методам,
которые включают пред- и постусловия, предикаты подтипов и инварианты типов.
Мы изучаем эти темы в этой главе.

Пред- и постусловия
-------------------

Пред- и постусловия обеспечивают ожидания
относительно входных и выходных параметров подпрограмм и возвращаемого
значения функций. Если мы говорим, что перед вызовом подпрограммы :ada:`Op`
должны быть выполнены определенные требования, это предварительные
условия. Точно так же, если определенные требования должны быть
выполнены после вызова подпрограммы :ada:`Op`, это постусловия. Мы можем
думать о предусловиях и постусловиях как об обещаниях между вызывающим и
вызываемым объектом подпрограммы: предусловие ‑ это обещание от
вызывающего объекта к вызываемому, а постусловие ‑ это обещание в
другом направлении.

Пред- и постусловия указываются с помощью предложения аспекта в
объявлении подпрограммы. Предложение :ada:`with Pre => <condition>`
определяет предварительное условие, а предложение
:ada:`with Post => <condition>` определяет постусловие.

В следующем коде показан пример предварительных условий:

.. code-block:: ada
    :class: ada-run-expect-failure

    procedure Show_Simple_Precondition is

       procedure DB_Entry (Name : String;
                           Age  : Natural)
         with Pre => Name'Length > 0
       is
       begin
          --  Missing implementation
          null;
       end DB_Entry;
    begin
       DB_Entry ("John", 30);

       --  Precondition will fail!
       DB_Entry ("",     21);
    end Show_Simple_Precondition;

В этом примере мы хотим, чтобы поле имени в нашей базе данных не
содержало пустой строки. Мы реализуем это требование, используя
предварительное условие, требующее, чтобы длина строки, используемой
для параметра :ada:`Name` процедуры :ada:`DB_Entry`, была больше нуля.
Если процедура :ada:`DB_Entry` вызывается с пустой строкой для параметра
:ada:`Name`, вызов завершится неудачно, поскольку предварительное условие
не выполнено.

.. admonition:: В наборе инструментов GNAT

    GNAT обрабатывает предварительные и постусловия, генерируя операторы
    для контроля во время выполнения программы. Однако по умолчанию
    утверждения не включены. Следовательно, чтобы проверять
    предварительные и постусловия во время выполнения, вам необходимо
    включить утверждения с помощью переключателя `-gnata`.

Прежде чем мы перейдем к следующему примеру, давайте кратко обсудим
количественные выражения, которые весьма полезны для краткого
написания предварительных и постусловий. Количественные выражения
возвращают логическое значение, указывающее, соответствуют ли элементы
массива или контейнера ожидаемому условию. Они имеют форму:
:ada:`(for all I in A'Range => <condition on A(I)>`, где
:ada:`A` ‑ это массив, а :ada:`I` ‑ индекс. Количественные выражения,
использующие :ada:`for all`, проверяют, выполняется ли условие для каждого
элемент. Например:

.. code-block:: ada
    :class: ada-nocheck

    (for all I in A'Range => A (I) = 0)

Это количественное выражение истинно только тогда, когда все элементы
массива :ada:`A` имеют нулевое значение.

Для некоторых используется другой вид (:ada:`for some`) количественных
выражений. Форма выглядит примерно так:
:ada:`(for some I in A'Range => <condition on A(I)>`. Однако в этом случае
квалифицированное выражение проверяет, истинно ли условие только для
некоторых (*some*) элементов (отсюда и название), а не для всех элементы.

Проиллюстрируем постусловия на следующем примере:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Simple_Postcondition is

       type Int_8 is range -2 ** 7 .. 2 ** 7 - 1;

       type Int_8_Array is
         array (Integer range <>) of Int_8;

       function Square (A : Int_8) return Int_8 is
         (A * A)
         with Post => (if abs A in 0 | 1
                       then Square'Result = abs A
                       else Square'Result > A);

       procedure Square (A : in out Int_8_Array)
         with Post => (for all I in A'Range =>
                         A (I) = A'Old (I) * A'Old (I))
       is
       begin
          for V of A loop
             V := Square (V);
          end loop;
       end Square;

       V : Int_8_Array := (-2, -1, 0, 1, 10, 11);
    begin
       for E of V loop
          Put_Line ("Original: "
                    & Int_8'Image (E));
       end loop;
       New_Line;

       Square (V);
       for E of V loop
          Put_Line ("Square:   "
                    & Int_8'Image (E));
       end loop;
    end Show_Simple_Postcondition;

Мы объявляем 8-битный тип со знаком :ada:`Int_8` и массив этого типа
(:ada:`Int_8_Array`). Мы хотим убедиться, что каждый элемент массива
возведен в квадрат после вызова процедуры :ada:`Square` для объекта
типа :ada:`Int_8_Array`. Мы делаем это с помощью постусловия, используя
выражение :ada:`for all`. Это постусловие также использует атрибут
:ada:`'Old` для ссылки на исходное значение параметра (до вызова).

Мы также хотим убедиться, что результат вызовов функции :ada:`Square`
для типа :ada:`Int_8` больше, чем вход для этого вызова. Для этого мы
пишем постусловие, используя атрибут :ada:`'Result` функции и сравнивая
его с входным значением.

Мы можем использовать как предварительные, так и постусловия в
объявлении одной подпрограммы. Например:

.. code-block:: ada
    :class: ada-run-expect-failure

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Simple_Contract is

       type Int_8 is range -2 ** 7 .. 2 ** 7 - 1;

       function Square (A : Int_8) return Int_8 is
         (A * A)
         with
              Pre  => (Integer'Size >= Int_8'Size * 2
                       and Integer (A) * Integer (A) <
                           Integer (Int_8'Last)),
              Post => (if abs A in 0 | 1
                       then Square'Result = abs A
                       else Square'Result > A);

       V : Int_8;
    begin
       V := Square (11);
       Put_Line ("Square of 11 is " & Int_8'Image (V));

       --  Precondition will fail...
       V := Square (12);
       Put_Line ("Square of 12 is " & Int_8'Image (V));
    end Show_Simple_Contract;

В этом примере мы хотим убедиться, что входное значение вызовов
функции :ada:`Square` для типа :ada:`Int_8` не вызовет переполнения в
этой функции. Мы делаем это путем преобразования входного значения в
тип :ada:`Integer`, который используется для временного вычисления,
и проверяем, находится ли результат в соответствующем диапазоне для
типа :ada:`Int_8`. В этом примере у нас то же постусловие, что и в
предыдущем.

Предикаты
---------

Предикаты определяют ожидания относительно типов. Они похожи на
предусловия и постусловия, но применяются к типам, а не к
подпрограммам. Их условия проверяются для каждого объекта данного
типа, что позволяет убедиться, что объект типа :ada:`T` соответствует
требованиям своего типа.

Есть два вида предикатов: статические и динамические. Проще говоря,
статические предикаты используются для проверки объектов во время
компиляции, а динамические предикаты используются для проверок во
время выполнения. Обычно статические предикаты используются для
скалярных типов и динамические предикаты для более сложных типов.

Статические и динамические предикаты указываются с помощью следующих
предложений соответственно:

-  :ada:`with Static_Predicate => <property>`

-  :ada:`with Dynamic_Predicate => <property>`

Давайте воспользуемся следующим примером, чтобы проиллюстрировать
динамические предикаты:

.. code-block:: ada
    :class: ada-run-expect-failure

    with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
    with Ada.Calendar;          use Ada.Calendar;
    with Ada.Containers.Vectors;

    procedure Show_Dynamic_Predicate_Courses is

       package Courses is
          type Course_Container is private;

          type Course is record
             Name       : Unbounded_String;
             Start_Date : Time;
             End_Date   : Time;
          end record
            with Dynamic_Predicate =>
              Course.Start_Date <= Course.End_Date;

          procedure Add (CC : in out Course_Container;
                         C  :        Course);
       private
          package Course_Vectors is new
            Ada.Containers.Vectors
              (Index_Type   => Natural,
               Element_Type => Course);

          type Course_Container is record
             V : Course_Vectors.Vector;
          end record;
       end Courses;

       package body Courses is
          procedure Add (CC : in out Course_Container;
                         C  :        Course) is
          begin
             CC.V.Append (C);
          end Add;
       end Courses;

       use Courses;

       CC : Course_Container;
    begin
       Add (CC,
            Course'(
              Name       => To_Unbounded_String
                             ("Intro to Photography"),
              Start_Date => Time_Of (2018, 5, 1),
              End_Date   => Time_Of (2018, 5, 10)));

       --  This should trigger an error in the
       --  dynamic predicate check
       Add (CC,
            Course'(
              Name       => To_Unbounded_String
                             ("Intro to Video Recording"),
              Start_Date => Time_Of (2019, 5, 1),
              End_Date   => Time_Of (2018, 5, 10)));

    end Show_Dynamic_Predicate_Courses;

В этом примере пакет :ada:`Courses` определяет тип :ada:`Course` и тип
:ada:`Course_Container`, объект которого содержит
все курсы. Мы хотим обеспечить согласованность дат каждого курса, в
частности, чтобы дата начала не позже даты окончания. Чтобы обеспечить
соблюдение этого правила, мы объявляем динамический предикат для типа
:ada:`Course`,который выполняет проверку для каждого объекта. Предикат
использует имя типа, в котором обычно используется переменная этого типа:
это ссылка на экземпляр тестируемого объекта.

Обратите внимание, что в приведенном выше примере используются
неограниченные строки (unbounded strings) и даты (dates). Оба типа
доступны в стандартной библиотеке Ada. Пожалуйста, обратитесь к
следующим разделам для получения дополнительной информации:

-  о типе неограниченной строки (:ada:`Unbounded_String`): раздел
   :ref:`Неограниченные строки <UnboundedStrings>`;

-  о дате и времени: Раздел :ref:`Даты и время <DatesTimes>`.

Статические предикаты, как упоминалось выше, в основном используются
для скалярных типов и проверяются во время компиляции. Они особенно
полезны для представления несмежных элементов перечисления.
Классический пример ‑ список дней недели:

.. code-block:: ada

    type Week is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);

Мы можем легко создать подсписок рабочих дней в неделе, указав подтип
(:ada:`subtype`) с диапазоном на основе :ada:`Week`. Например:

.. code-block:: ada

    subtype Work_Week is Week range Mon .. Fri;

Диапазоны в Ada могут быть указаны только как непрерывные списки: они
не позволяют нам выбирать определенные дни. Однако мы можем захотеть
создать список, содержащий только первый, средний и последний день
рабочей недели. Для этого мы используем статический предикат:

.. code-block:: ada

    subtype Check_Days is Work_Week
      with Static_Predicate => Check_Days in Mon | Wed | Fri;

Давайте посмотрим на полный пример:

.. code-block:: ada
    :class: ada-run-expect-failure

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Predicates is

       type Week is (Mon, Tue, Wed, Thu,
                     Fri, Sat, Sun);

       subtype Work_Week is Week range Mon .. Fri;

       subtype Test_Days is Work_Week
         with Static_Predicate =>
           Test_Days in Mon | Wed | Fri;

       type Tests_Week is array (Week) of Natural
         with Dynamic_Predicate =>
           (for all I in Tests_Week'Range =>
              (case I is
                   when Test_Days =>
                      Tests_Week (I) > 0,
                   when others    =>
                      Tests_Week (I) = 0));

       Num_Tests : Tests_Week :=
                     (Mon => 3, Tue => 0,
                      Wed => 4, Thu => 0,
                      Fri => 2, Sat => 0,
                      Sun => 0);

       procedure Display_Tests (N : Tests_Week) is
       begin
          for I in Test_Days loop
             Put_Line ("# tests on "
                       & Test_Days'Image (I)
                       & " => "
                       & Integer'Image (N (I)));
          end loop;
       end Display_Tests;

    begin
       Display_Tests (Num_Tests);

       --  Assigning non-conformant values to
       --  individual elements of the Tests_Week
       --  type does not trigger a predicate
       --  check:
       Num_Tests (Tue) := 2;

       --  However, assignments with the "complete"
       --  Tests_Week type trigger a predicate
       --  check. For example:
       --
       --  Num_Tests := (others => 0);

       --  Also, calling any subprogram with
       --  parameters of Tests_Week type
       --  triggers a predicate check. Therefore,
       --  the following line will fail:
       Display_Tests (Num_Tests);
    end Show_Predicates;

Здесь у нас есть приложение, которое хочет проводить тесты только в
три дня рабочей недели. Эти дни указаны в подтипе :ada:`Test_Days`. Мы хотим
отслеживать количество тестов, которые проводятся каждый день. Мы
объявляем тип :ada:`Tests_Week` как массив, объект которого будет содержать
количество тестов, выполняемых каждый день. Согласно нашим требованиям,
эти тесты должны проводиться только в вышеупомянутые три дня; в другие дни
никаких анализов проводить не следует. Это требование реализовано с
помощью динамического предиката типа :ada:`Tests_Week`. Наконец, фактическая
информация об этих тестах хранится в массиве :ada:`Num_Tests`, который
является экземпляром типа :ada:`Tests_Week`.

Динамический предикат типа :ada:`Tests_Week` проверяется во время
инициализации :ada:`Num_Tests`. Если
у нас там будет несоответствующее значение, проверка не удастся.
Однако, как мы видим в нашем примере, отдельные присвоения элементам
массива не запускают проверку. На этом этапе мы не можем проверить
согласованность, потому что инициализация сложной структуры данных
(например, массивов или записей) не может выполняться с одним
присваиванием. Однако, как только объект передается в качестве
аргумента подпрограмме, динамический предикат проверяется, потому что
подпрограмма требует, чтобы объект был согласован. Это происходит при
последнем вызове :ada:`Display_Tests` в нашем примере. Здесь проверка
предиката не выполняется, потому что предыдущее присвоение имеет
несоответствующее значение.

Инварианты типа
---------------

Инварианты типов ‑ это еще один способ определения ожиданий в
отношении типов. В то время как предикаты используются для не
*частных типов*, инварианты типов используются исключительно
для определения
ожиданий в отношении частных типов. Если тип :ada:`T` из пакета :ada:`P`
имеет инвариант типа, результаты операций с объектами типа :ada:`T`
всегда согласуются с этим инвариантом.

Инварианты типов указываются с помощью предложения
:ada:`with Type_Invariant => <property>`. Подобно
предикатам, *свойство* определяет условие, которое позволяет нам
проверить, соответствует ли объект типа :ada:`T` его требованиям. В этом
смысле инварианты типов можно рассматривать как своего рода предикат
для частных типов. Однако есть некоторые отличия в плане проверок. В
следующей таблице приведены различия:

+------------+------------------------------+-------------------------+
| Элемент    | Проверки параметров          | Проверки присвоения     |
|            | подпрограммы                 |                         |
+============+==============================+=========================+
| Предикаты  | По всем входящим :ada:`in`   | При назначениях и явных |
|            | и выходящим :ada:`out`       | инициализациях          |
|            | параметрам                   |                         |
+------------+------------------------------+-------------------------+
| Инварианты | По параметрам :ada:`out`,    | При всех инициализациях |
| типов      | возвращенным из подпрограмм, |                         |
|            | объявленных в той же         |                         |
|            | общедоступной области        |                         |
+------------+------------------------------+-------------------------+

Мы могли бы переписать наш предыдущий пример и заменить динамические
предикаты инвариантами типов. Это выглядело бы так:

.. code-block:: ada
    :class: ada-run-expect-failure

    with Ada.Text_IO;           use Ada.Text_IO;
    with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
    with Ada.Calendar;          use Ada.Calendar;
    with Ada.Containers.Vectors;

    procedure Show_Type_Invariant is

       package Courses is
          type Course is private
            with Type_Invariant => Check (Course);

          type Course_Container is private;

          procedure Add (CC : in out Course_Container;
                         C  :        Course);

          function Init
            (Name                 : String;
             Start_Date, End_Date : Time) return Course;

          function Check (C : Course) return Boolean;

       private
          type Course is record
             Name       : Unbounded_String;
             Start_Date : Time;
             End_Date   : Time;
          end record;

          function Check (C : Course) return Boolean is
            (C.Start_Date <= C.End_Date);

          package Course_Vectors is new
            Ada.Containers.Vectors
              (Index_Type   => Natural,
               Element_Type => Course);

          type Course_Container is record
             V : Course_Vectors.Vector;
          end record;
       end Courses;

       package body Courses is
          procedure Add (CC : in out Course_Container;
                         C  :        Course) is
          begin
             CC.V.Append (C);
          end Add;

          function Init
            (Name                 : String;
             Start_Date, End_Date : Time) return Course is
          begin
             return
               Course'(Name       => To_Unbounded_String (Name),
                       Start_Date => Start_Date,
                       End_Date   => End_Date);
          end Init;
       end Courses;

       use Courses;

       CC : Course_Container;
    begin
       Add (CC,
            Init (Name       => "Intro to Photography",
                  Start_Date => Time_Of (2018, 5, 1),
                  End_Date   => Time_Of (2018, 5, 10)));

       --  This should trigger an error in the
       --  type-invariant check
       Add (CC,
            Init (Name       => "Intro to Video Recording",
                  Start_Date => Time_Of (2019, 5, 1),
                  End_Date   => Time_Of (2018, 5, 10)));
    end Show_Type_Invariant;

Основное отличие состоит в том, что тип :ada:`Course` был видимым (открытым)
типом пакета :ada:`Courses` в предыдущем примере, но в этом примере является
частным типом.
