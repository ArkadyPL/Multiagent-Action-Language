# Multiagent-Action-Language
To do:

### Prolog ###
- [ ] Obsługa listy fluentów przez *always*
- [ ] Napisanie testów dla bardziej zaawansowanych historyjek, w szczególności z wykorzystaniem *releases*, *initially*, *after* i *always*
- [ ] Ujednolicenie sposobu wpisywania wyrażeń logicznych
- [ ] Testy do *Possibly executable from*
- [x] Pierwsza wersja kwerendy *Possibly engagnd from*
- [x] Pierwsza wersja kwerendy *Necessary engaged from*
- [x] Pierwsza wersja kwerendy *Possibly executable from*
- [x] Pierwsza wersja kwerendy *Necessary executable from*
- [ ] Pierwsza wersja kwerendy *Possibly after from*
- [x] Pierwsza wersja kwerendy *Necessary after from*

### C# GUI ###
- [x] Parser na wyrażenia wartości *after*, *observable after*, *initially*
- [x] Parser na wyrażenia efektu akcji *by causes if*, *impossible by if*
- [x] Parser na wyrażenia uwolnienia fluentu *by releases if*
- [x] Parser na wyrażenia integralności *always*
- [x] Parser na wyrażenie zachowania fluentów *noninertial*
- [ ] Parser na kwerendę wykonywalności *executable*
- [ ] Parser na kwerendę wartości *after*
- [ ] Parser na kwerendę zaangażowania *engaged*
- [x] Parser wyrażeń logicznych do obiektów
- [x] Widoki dla kontrolek
- [x] Parser tworzenia agentów

### C# logika ###
- [ ] Testy
- [x] Działający serwis C# <-> Prolog
- [x] Wrappery na wyrażenia wartości *after*, *observable after*, *initially*
- [x] Wrappery na wyrażenia efektu akcji *by causes if*, *impossible by if*
- [x] Wrappery na wyrażenia uwolnienia fluentu *by releases if*
- [x] Wrappery na wyrażenia integralności *always*
- [x] Wrapper na wyrażenie zachowania fluentów *noninertial*
- [x] Wrappery na kwerendę wykonywalności *executable*
- [x] Wrappery na kwerendę wartości *after*
- [x] Wrappery na kwerendę zaangażowania *engaged*
- [x] Parser wyrażeń logicznych do formatu prologowego, z uwzględnieniem wszystkich kombinacji fluentów spełniających dane wyrażenie
- [x] Dodanie stanów początkowych