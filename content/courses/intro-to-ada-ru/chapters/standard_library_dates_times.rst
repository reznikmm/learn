Стандартная библиотека: Дата и время
====================================

.. include:: ../../global.txt

Стандартная библиотека поддерживает обработку дат и времени с
использованием двух подходов:

-  *Календарный* подход, подходящий для обработки дат и времени в целом;

-  Подход *реального времени*, который лучше подходит для приложений в
   реальном времени, требующих повышенной точности, например, благодаря
   доступу к абсолютным часам и интервалам времени обработки. Следует
   отметить, что этот подход поддерживает только время, а не даты.

Эти два подхода представлены в следующих разделах.

.. _DatesTimes:

Обработка даты и времени
------------------------

Пакет :ada:`Ada.Calendar` поддерживает обработку дат и времени. Рассмотрим
простой пример:

.. code-block:: ada

    with Ada.Calendar;            use Ada.Calendar;
    with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
    with Ada.Text_IO;             use Ada.Text_IO;

    procedure Display_Current_Time is
       Now : Time := Clock;
    begin
       Put_Line ("Current time: " & Image (Now));
    end Display_Current_Time;

В этом примере отображаются текущая дата и время, которые извлекаются
при вызове функции :ada:`Clock`. Мы вызываем функцию :ada:`Image` из пакета
:ada:`Ada.Calendar.Formatting`, чтобы получить
строку (:ada:`String`) для текущей даты и времени. Вместо этого мы могли бы
получить каждый компонент с помощью функции :ada:`Split`. Например:

.. code-block:: ada

    with Ada.Calendar;            use Ada.Calendar;
    with Ada.Text_IO;             use Ada.Text_IO;

    procedure Display_Current_Year is
       Now         : Time := Clock;

       Now_Year    : Year_Number;
       Now_Month   : Month_Number;
       Now_Day     : Day_Number;
       Now_Seconds : Day_Duration;
    begin
       Split (Now,
              Now_Year,
              Now_Month,
              Now_Day,
              Now_Seconds);

       Put_Line ("Current year  is: "
                 & Year_Number'Image (Now_Year));
       Put_Line ("Current month is: "
                 & Month_Number'Image (Now_Month));
       Put_Line ("Current day   is: "
                 & Day_Number'Image (Now_Day));
    end Display_Current_Year;

Здесь мы получаем каждый элемент и отображаем его отдельно.

Задержка с использованием даты
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Вы можете отложить запуск приложения, чтобы оно перезапустилось в
определенную дату и время. Мы видели нечто подобное в главе о задачах.
Вы делаете это с помощью инструкции :ada:`delay until`. Например:

.. code-block:: ada

    with Ada.Calendar;            use Ada.Calendar;
    with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
    with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
    with Ada.Text_IO;             use Ada.Text_IO;

    procedure Display_Delay_Next_Specific_Time is
       TZ   : Time_Offset := UTC_Time_Offset;
       Next : Time        :=
         Ada.Calendar.Formatting.Time_Of
           (Year        => 2018,
            Month       => 5,
            Day         => 1,
            Hour        => 15,
            Minute      => 0,
            Second      => 0,
            Sub_Second  => 0.0,
            Leap_Second => False,
            Time_Zone   => TZ);

       --  Next = 2018-05-01 15:00:00.00
       --         (local time-zone)
    begin
       Put_Line ("Let's wait until...");
       Put_Line (Image (Next, True, TZ));

       delay until Next;

       Put_Line ("Enough waiting!");
    end Display_Delay_Next_Specific_Time;

В этом примере мы указываем дату и время, инициализируя :ada:`Next` с помощью
вызова :ada:`Time_Of`, функции, принимающей различные компоненты даты (год,
месяц и т. д.) и возвращающей элемент типа :ada:`Time`. Поскольку указанная
дата находится в прошлом, задержка :ada:`delay until` до выражения не даст
заметного эффекта. Если мы пропустим дату в будущем, программа будет ждать
наступления этой конкретной даты и времени.

Здесь мы переводим время в местный часовой пояс. Если мы не указываем
часовой пояс, по умолчанию используется всемирное координированное
время (*Coordinated Universal Time* сокращенно UTC). Получив смещение времени
к UTC с помощью вызова :ada:`UTC_Time_Offset` из пакета
:ada:`Ada.Calendar.Time_Zones`, мы можем инициализировать :ada:`TZ` и
использовать его при вызове :ada:`Time_Of`. Это все, что нам нужно сделать,
чтобы информация, предоставляемая :ada:`Time_Of`, относилась к местному
часовому поясу.

Мы могли бы добиться аналогичного результата, инициализировав :ada:`Next` с
помощью :ada:`String`. Мы можем сделать это с помощью вызова :ada:`Value` из
пакета :ada:`Ada.Calendar.Formatting`. Это модифицированный код:

.. code-block:: ada

    with Ada.Calendar;            use Ada.Calendar;
    with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
    with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
    with Ada.Text_IO;             use Ada.Text_IO;

    procedure Display_Delay_Next_Specific_Time is
       TZ   : Time_Offset := UTC_Time_Offset;
       Next : Time        :=
         Ada.Calendar.Formatting.Value
           ("2018-05-01 15:00:00.00", TZ);

       --  Next = 2018-05-01 15:00:00.00
       --         (local time-zone)
    begin
       Put_Line ("Let's wait until...");
       Put_Line (Image (Next, True, TZ));

       delay until Next;

       Put_Line ("Enough waiting!");
    end Display_Delay_Next_Specific_Time;

В этом примере мы снова используем :ada:`TZ` в вызове :ada:`Value`, чтобы
настроить время ввода в соответствии с текущим часовым поясом.

В приведенных выше примерах мы задерживались до определенной даты и
времени. Как мы видели в главе о задачах, мы могли бы вместо этого
указать задержку относительно текущего времени. Например, мы можем
задержать на 5 секунд, используя текущее время:

.. code-block:: ada

    with Ada.Calendar;            use Ada.Calendar;
    with Ada.Text_IO;             use Ada.Text_IO;

    procedure Display_Delay_Next is
       D    : Duration := 5.0;
       --                 ^ seconds
       Now  : Time     := Clock;
       Next : Time     := Now + D;
       --                       ^ use duration to
       --                         specify next point
       --                         in time
    begin
       Put_Line ("Let's wait "
                 & Duration'Image (D) & " seconds...");
       delay until Next;
       Put_Line ("Enough waiting!");
    end Display_Delay_Next;

Здесь мы указываем продолжительность 5 секунд в :ada:`D`, добавляем ее к
текущему времени из :ada:`Now` и сохраняем сумму в :ada:`Next`. Затем мы
используем его в операторе :ada:`delay until` ‑ "Задержка до".

В режиме реального времени
--------------------------

В дополнение к :ada:`Ada.Calendar` стандартная библиотека также поддерживает
операции со временем для приложений реального времени. Они включены в пакет
:ada:`Ada.Real_Time`. Этот пакет также включает тип :ada:`Time`. Однако в
пакете :ada:`Ada.Real_Time` тип :ada:`Time` используется для
представления абсолютных часов и обработки промежутка времени. Это
контрастирует с :ada:`Ada.Calendar`, который использует тип :ada:`Time` для
представления даты и времени.

В предыдущем разделе мы использовали тип :ada:`Time` из :ada:`Ada.Calendar` и
оператор :ada:`delay until`, чтобы отложить приложение на 5 секунд. Вместо
этого мы могли бы использовать пакет :ada:`Ada.Real_Time`. Давайте изменим этот
пример:

.. code-block:: ada

    with Ada.Text_IO;   use Ada.Text_IO;
    with Ada.Real_Time; use Ada.Real_Time;

    procedure Display_Delay_Next_Real_Time is
       D     : Time_Span := Seconds (5);
       Next  : Time      := Clock + D;
    begin
       Put_Line ("Let's wait "
                 & Duration'Image (To_Duration (D))
                 & " seconds...");
       delay until Next;
       Put_Line ("Enough waiting!");
    end Display_Delay_Next_Real_Time;

Основное отличие состоит в том, что :ada:`D` теперь является переменной типа
:ada:`Time_Span`, определенной в пакете :ada:`Ada.Real_Time`. Мы вызываем
функцию :ada:`Seconds` для инициализации :ada:`D`, но мы могли бы получить
более тонкую детализацию, вызвав вместо этого :ada:`Nanoseconds`. Кроме того,
нам нужно сначала преобразовать :ada:`D` в тип :ada:`Duration` с помощью
функции :ada:`To_Duration`, прежде чем мы сможем его отобразить.

Сравнительный анализ
~~~~~~~~~~~~~~~~~~~~

Одним из интересных приложений, использующих пакет :ada:`Ada.Real_Time`, 
является сравнительный анализ. Мы уже использовали этот пакет в предыдущем
разделе при обсуждении задач. Давайте рассмотрим пример сравнительного
анализа:

.. code-block:: ada

    with Ada.Text_IO;   use Ada.Text_IO;
    with Ada.Real_Time; use Ada.Real_Time;

    procedure Display_Benchmarking is

       procedure Computational_Intensive_App is
       begin
          delay 5.0;
       end Computational_Intensive_App;

       Start_Time, Stop_Time : Time;
       Elapsed_Time          : Time_Span;

    begin
       Start_Time := Clock;

       Computational_Intensive_App;

       Stop_Time    := Clock;
       Elapsed_Time := Stop_Time - Start_Time;

       Put_Line ("Elapsed time: "
                 & Duration'Image (To_Duration (Elapsed_Time))
                 & " seconds");
    end Display_Benchmarking;

В этом примере определяется фиктивное приложение
:ada:`Computational_Intensive_App`, реализованное с использованием простого
оператора задержки :ada:`delay`. Мы инициализируем :ada:`Start_Time` и
:ada:`Stop_Time` по текущим на тот момент часам и вычисляем прошедшее время.
Запустив эту программу, мы видим, что время составляет примерно 5 секунд, что
ожидается из-за оператора задержки :ada:`delay`.

Аналогичное приложение ‑ это сравнительный анализ процессорного
времени. Мы можем реализовать это с помощью пакета :ada:`Execution_Time`.
Давайте изменим предыдущий пример, чтобы измерить процессорное время:

.. code-block:: ada

    with Ada.Text_IO;        use Ada.Text_IO;
    with Ada.Real_Time;      use Ada.Real_Time;
    with Ada.Execution_Time; use Ada.Execution_Time;

    procedure Display_Benchmarking_CPU_Time is

       procedure Computational_Intensive_App is
       begin
          delay 5.0;
       end Computational_Intensive_App;

       Start_Time, Stop_Time : CPU_Time;
       Elapsed_Time          : Time_Span;

    begin
       Start_Time := Clock;

       Computational_Intensive_App;

       Stop_Time    := Clock;
       Elapsed_Time := Stop_Time - Start_Time;

       Put_Line ("CPU time: "
                 & Duration'Image (To_Duration (Elapsed_Time))
                 & " seconds");
    end Display_Benchmarking_CPU_Time;

В этом примере :ada:`Start_Time` и :ada:`Stop_Time` имеют тип :ada:`CPU_Time`
вместо :ada:`Time`. Однако мы по-прежнему вызываем функцию :ada:`Clock` для
инициализации обеих переменных и вычисления прошедшего времени так же, как и
раньше. Запустив эту программу, мы видим, что время процессора значительно
ниже, чем те 5 секунд, которые мы видели раньше. Это связано с тем, что
оператор задержки :ada:`delay` не требует много времени процессора.
Результаты будут другими, если мы изменим реализацию
:ada:`Computational_Intensive_App` для использования математических функций
в длинном цикле. Например:

.. code-block:: ada

    with Ada.Text_IO;        use Ada.Text_IO;
    with Ada.Real_Time;      use Ada.Real_Time;
    with Ada.Execution_Time; use Ada.Execution_Time;

    with Ada.Numerics.Generic_Elementary_Functions;

    procedure Display_Benchmarking_Math is

       procedure Computational_Intensive_App is
          package Funcs is new Ada.Numerics.Generic_Elementary_Functions
            (Float_Type => Long_Long_Float);
          use Funcs;

          X : Long_Long_Float;
       begin
          for I in 0 .. 1_000_000 loop
             X := Tan (Arctan
                       (Tan (Arctan
                          (Tan (Arctan
                             (Tan (Arctan
                                (Tan (Arctan
                                   (Tan (Arctan
                                      (0.577))))))))))));
          end loop;
       end Computational_Intensive_App;

       procedure Benchm_Elapsed_Time is
          Start_Time, Stop_Time : Time;
          Elapsed_Time          : Time_Span;

       begin
          Start_Time := Clock;

          Computational_Intensive_App;

          Stop_Time    := Clock;
          Elapsed_Time := Stop_Time - Start_Time;

          Put_Line ("Elapsed time: "
                    & Duration'Image (To_Duration (Elapsed_Time))
                    & " seconds");
       end Benchm_Elapsed_Time;

       procedure Benchm_CPU_Time is
          Start_Time, Stop_Time : CPU_Time;
          Elapsed_Time          : Time_Span;

       begin
          Start_Time := Clock;

          Computational_Intensive_App;

          Stop_Time    := Clock;
          Elapsed_Time := Stop_Time - Start_Time;

          Put_Line ("CPU time: "
                    & Duration'Image (To_Duration (Elapsed_Time))
                    & " seconds");
       end Benchm_CPU_Time;
    begin
       Benchm_Elapsed_Time;
       Benchm_CPU_Time;
    end Display_Benchmarking_Math;

Теперь, когда наша фиктивная :ada:`Computational_Intensive_App` включает
математические операции, требующие значительного времени ЦП, измеренное
затраченное время и время ЦП намного ближе друг к другу, чем раньше.
