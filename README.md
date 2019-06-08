﻿# Multiagent-Action-Language
To do:

### C# GUI ###
- [ ] BŁĄD w Test3_PARSER_ERROR brak parsowania dla *impossible by*
- [ ] Podmiana *not by if* na odpowiednik *impossible by if*
- [ ] Parser na wyrażenie *not by if*
- [ ] Pusta lista agentów
- [ ] Dodać *initially [alpha]* jako *[alpha] after ()* (chodzi o puste wykonanie programu)

### C# reasoning ###
- [ ] Dodanie obsługi *[alpha] observable after ...* i *[alpha] after ...*
- [ ] Testy generowania grafu
- [x] Dodać kwerendę wykonywalności *executable*
- [x] Dodać kwerendę wartości *after*
- [ ] Dodać kwerendę zaangażowania *engaged*
- [x] Dodać testy języka AR (przykłady z wykładu)
- [x] Dodać kwerendy z pustego programu
- [x] Dodać kwerendy dla pustej listy agentów

### App tests ###
- [ ] 4 historyjki po 3-4 kwerendy dla grupy 1 **na poniedziałek musi być gotowe**
- [ ] kilka historyjek po 3-4 kwerendy do naszych testów

### Requirements for the deadline ###
- Dokumentacja teoretyczna
- Dokumentacja techniczna
- User Guide
- Testy naszego programu zrobione przez inną grupę **patrz wyżej**
- Testy innego programu zrobione przez nas
- Płytka CD

### Conclusions after deadline meeting ###
- kwerendy mogą też dostać wyrażenie logiczne jako resultat - co wtedy?
- program niewykonalny -> necessary true
- niedeterminizm - releases itp nie działa


Przykład niedziałającego modelu:
a by [x] causes [f]
a by [y] causes [g]
initially [~f && ~g]
Query:
necessary (f && g) after  (a, [x]), (b, [y])
Should be: true


Przykład niedziałającego modelu:
a causes [f]
a causes [g]
initially [~f && ~g]
Query:
necessary (f && g) after  (a, [x]), (b, [y])
Should be: true


Przykład działającego modelu:
a by [x] causes [f]
b by [x] causes [g]
initially [~f && ~g]
Query:
necessary (f && g) after  (a, [x]), (b, [x])
Should be: true


Przykład niedziałającego modelu:
fire causes [~loaded]
fire causes [~alive] if [loaded]
initially [alive]
[~alive] after (fire, [x]) % tu powinno być bez agenta
Query:
necessary <coś> after [] % to powinno być możliwe do sprawdzenia
Should be: true


Przykład niedziałającego modelu:
cos causes [ble]
fire causes [~loaded]
fire causes [~alive] if [loaded]
initially [alive]
[~alive] after (cos, [x])m (fire, [x])
Query:
necessary (loaded) after (cos, [x])
Should be: true
