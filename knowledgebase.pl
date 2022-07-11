% Facts copied for implementation of test cases.

age(rohan, 23).
age(meera, 30).
age(david, 35).
age(leonard, 40).
age(amy, 38).

citizen(rohan, 23).
citizen(meera, 8).
citizen(david, 35).
citizen(leonard, 40).
citizen(amy, 5).

stateOfUS(newHampshire).
stateOfUS(massachusetts).
stateOfUS(connecticut).
stateOfUS(newYork).
stateOfUS(newJersey).
stateOfUS(pennsylvania).
stateOfUS(delaware).
stateOfUS(maryland).
stateOfUS(virginia).
stateOfUS(northCarolina).
stateOfUS(southCarolina).
stateOfUS(georgia).

monday(7, 1, 2019).
monday(2, 12, 2019).

% PREAMBLE

preamble('We the People of the United States, in Order to form a more perfect Union, establish Justice, insure domestic Tranquility, provide for the common defence, promote the general Welfare, and secure the Blessings of Liberty to ourselves and our Posterity, do ordain and establish this Constitution for the United States of America.').

% ARTICLE 1

% Section 1

power(congress,power(legislative)).
congressHas(senate).
congressHas(houseOfRepresentatives).

% Section 2

tenure(houseOfRepresentatives,2).

electorFrom(X):- stateOfUS(X).
fulfill(qualifications(elector)).
residentState(houseOfRepresentatives,X) :- stateOfUS(X).
electedBy(houseOfRepresentatives,elector(X)):- electorFrom(X),residentState(houseOfRepresentatives,X).

qualified(X, houseOfRepresentatives) :- age(X,A), citizen(X,C),A>=25,C>= 7.

/*Original functor due to section 2 of article 1.

Statepercentage represents the Representatives and direct Taxes shall be
apportioned to a particular state with respect to total Representatives
and direct taxes.

Total represents the total number of people.

divisionOfTaxandRepresentativesOfState(Statepercentage,Total,FreePeople,Indians,OtherPeople):-
Y is FreePeople - Indians, Z_1 is 0.6, Z_2 is OtherPeople*Z_1, Z_3 is
Y+Z_2, Statepercentage is Z_3/Total.

people(indians,not(taxed)). */

%-------------Changed due to section 2 of amendment 14----------

divisionOfTaxandRepresentativesOfState(Statepercentage,Total,FreePeople,Indians):- Y is FreePeople - Indians, Statepercentage is Y/Total.
people(indians,not(taxed)).
people(deniedVote,maleInhabitant(twentyone(participation(rebellion)))).
reduceProportion(stateRepresentation,DeniedVote,TotalMale_TwentyOneYears):- DeniedVote/TotalMale_TwentyOneYears.

%changes due to amendment 26 section 1
right(X,rightToVote(eighteenOrOlder(notDenied(onBasisOfAge)))):-citizen(X,_).

%---------------------------------------------------------------

isEnumerationRequired(YearsSinceLastEnumeration):-YearsSinceLastEnumeration==10. %Every subsequent 10 years the enumeration is required.

isValidRepresentative(Population,NumofStateRepresentatives):- Population>=30000->NumofStateRepresentatives is Population div 30000;NumofStateRepresentatives=1. %A representative is to be selected for every 30000 people and each state should have atleast one representative so representative.

% If enumeration is not made the list is given as ouput consting of the
% original list of votes given to each state.
isEnumerationMade(X,Valid):-X==true->Valid='Original list is invalid';Valid = [['New Hampshire',3],['Massachusetts',8], ['Rhode-Island',1], ['Providence Plantations',1], ['Connecticut',5], ['New-York',6], ['New Jersey',4], ['Pennsylvania',8], ['Delaware',1],['Maryland',6], ['Virginia',10], ['North Carolina',5], ['South Carolina',5],['Georgia',3]].

vacancy(houseOfRepresentatives,executive_authority(issue(writs(election)))).
power(houseOfRepresentatives,solepower(impeachment)).

canChooseSpeaker(houseOfRepresentatives,true).
canChooseOfficers(houseOfRepresentatives,true).

% Section 3

tenure(senator,6).
isValidSenator(NumofStateSenators):- NumofStateSenators =< 2. %A state is to choose two senators.
vote(senator,1).
classes(senate,3).

/*

removed classes due to amendment 17
X represents class number
If senator is of first class he has to vacate after 2 years
If senator is of first class he has to vacate after 4 years
If senator is of first class he has to vacate after 6 years
vacationAccToClass(X,Y):-X==1->Y = 2;X==2-> Y=4; Y=6.

Class X shall be vacated at end of Y years.

Original vacancy functor
vacancy(senate,temporary_appointment).

*/

/*

Original predicates according to section 3 article 1

stateLegislature(X):- stateOfUS(X).
residentState(senator,X) :- stateOfUS(X).
electedBy(senate,X):- stateLegislature(X),residentState(senator,X).

*/

% ---------changes due to amendment 17--------------------------
fulfill(qualifications(elector)).
residentState(senate,X) :- stateOfUS(X).
electedBy(senate,elector(X)):- electorFrom(X),residentState(senate,X).

vacancy(senate,executiveAuthority(state(writsOf(election)))).
untilVacancyByElection(senate,empower(executive(temporary_appointments))).
%---------------------------------------------------------------

qualified(X, senate) :- age(X,A), citizen(X,C),A>=30,C>= 9.

presidentOfSenate('Vice President').
presidentOfSenate_voteValue(EquallyDivided,X):-EquallyDivided==true->X=1;X=0.

canChoosePresidentOf(senate,false).
canChoosePresidentTempore(senate,true).
canChooseOfficers(senate,true).

power(senate,solepower(try(impeachments))).
try_impeachment(senator(oath)).

presideDuringImpeachment(senate,'Chief Justice').
% Percentage is the percentage of senators that are in favour of
% conviction
convict(Percentage,senate):-Percentage>0.67.

judgement(removal(office)).
judgement(disqualification_from(holdOffice(under(unitedStates)))).
judgement(convictedLiableTo(trailAndPunishment(accordingToLaw))).
judgementInImpeachment(X):- judgement(X).

% Section 4

stateLegislature(prescribe(times_places_manner(holdingElections( senatorsAndrepresentatives)))).
power(stateLegislature, X):- stateLegislature(X).
power(congress,law(regulations(elections))).
restriction(congress,law(places_choose(senators))).

shouldCongressAssemble(X):-X<1.

% original functor meetingOfCongress according to article 1 section 4
%meetingOfCongress(D, M, Z):-monday(D,M,Z),M==12,D=<7.

%----------changes due to amendment 20 section 2----------------
meetingOfCongress(D, M, Z):-D==3,M==1.
%---------------------------------------------------------------

% Section 5

power(senate,judge(elections_returns_qualifications(members))).
power(houseOfRepresentatives,judge(elections_returns_qualifications(members))).
power(senate,majority(constitute(quorum(business)))).
power(houseOfRepresentatives,majority(constitute(quorum(business)))).
power(senate,compel(attendance(absentMembers(underPenalities)))).
power(houseOfRepresentatives,compel(attendance(absentMembers(underPenalities)))).

power(senate,determine(rulesOf(proceedings))).
power(senate,determine(rulesTo(punish(members)))).
power(houseOfRepresentatives,determine(rulesOf(proceedings))).
power(houseOfRepresentatives,determine(rulesTo(punish(members)))).
power(senate,concurrence(twothirds(expel(member)))).
power(houseOfRepresentatives,concurrence(twothirds(expel(member)))).

maintain(senate,journalOf(proceedings)).
publish(senate,journalOf(proceedings(timeTotime))).
maintain(houseOfRepresentatives,journalOf(proceedings)).
publish(houseOfRepresentatives,journalOf(proceedings(timeTotime))).

power(senate,prevent(partsOf(proceedingsTo(publishIn(journal))))).
power(houseOfRepresentatives,prevent(partsOf(proceedingsTo(publishIn(journal))))).
power(senate,atDesireOf(onefifth(question(enteredIn(journal))))).
power(houseOfRepresentatives,atDesireOf(onefifth(question(enteredIn(journal))))).

power(senate,adjourn(houseOfRepresentatives(moreThan(threeDays)))).
power(senate,adjourn(houseOfRepresentatives(place(other)))).
power(houseOfRepresentatives,adjourn(senate(moreThan(threeDays)))).
power(houseOfRepresentatives,adjourn(senate(place(other)))).

% Section 6

compensationAscertained(law).
paid(treasury(unitedStates)).
compensation(By,Paid):-compensationAscertained(By),paid(Paid).

exception(except(treason)).
exception(except(felony)).
exception(except(breachOf(peace))).

previlage(senator,privelagedFrom(arrest(attendance(session))), Except):-exception(Except).
previlage(senator,privelagedFrom(arrest(goingTo_comingFrom(session))), Except):-exception(Except).
previlage(representative,privelagedFrom(arrest(attendance(session))), Except):-exception(Except).
previlage(representative,privelagedFrom(arrest(goingTo_comingFrom(session))), Except):-exception(Except).
previlage(senator,notQuestioned(speech(anyOther(place)))).
previlage(representative,notQuestioned(speech(anyOther(place)))).

restriction(senator,cannotAppointed(civilOffice(authorityUS(createdOrIncreased(tenure))))).
restriction(representative,cannotAppointed(civilOffice(authorityUS(createdOrIncreased(tenure))))).
restriction(personHoldingOffice,cannotBe(memberOf(congress(duringOffice)))).


% section 7

passedByhouseOfRepresentative(law1).
passedBySenate(law1).
signedByPresident(law1).

passedByhouseOfRepresentative(law2).
passedBySenate(law2).

objectedByPresident(law2).
returnedToHouse(law2).
reportedOnJournal(law2).

passedByhouseOfRepresentative(law3).
passedBySenate(law3).
days_exceeded(law3).

law(law1).
law(law2).
law(law3).
notLaw(law4).

isLaw(X) :-
	passedByhouseOfRepresentative(X),
	passedBySenate(X),
	signedByPresident(X) ;
	passedByhouseOfRepresentative(X),
	passedBySenate(X),
	forReconsidertion(X).


forReconsidertion(X) :-
	objectedByPresident(X),
	returnedToHouse(X),
	reportedOnJournal(X) ->
	getVotes ;
	days_exceeded(X).




getVotes :-
    write('Enter votes:'),nl,
    read(Vote),
	Vote is 'Y' -> append([Vote],[ListOfY],ListOfY);
		append([Vote],[ListOfN],ListOfN);
	write('Any new vote :(yes/no)'),nl,
	read(D),
	dif(D,stop),
	lawVotingResult(ListOfY,ListOfN).


lawVotingResult(L1 , L2) :-
	length(L2 , N) , length(L1 , Y),
	Y >((2 div 3 )* houseMember)-> law(X) ; notLaw(X).


% section 8

power(congress, lay_Taxes).
power(congress, collect_Taxes).
power(congress, collect_Duties).
power(congress, collect_Imposts).
power(congress, collect_Excises).

toPayFor([taxes, duties, imposts, excises] ,
	[debt, defence, general_welfare]).

usedFor([taxes, duties, imposts, excises],
	unitedStates(credit(borrow(money))) ).
usedFor([taxes, duties, imposts, excises],
	regulate(commerce(foreignNations))).
usedFor([taxes, duties, imposts, excises],
	regulate(commerce( severalStates))).
usedFor([taxes, duties, imposts, excises],
	regulate(commerce(indianTribes))).

usedFor([taxes, duties, imposts, excises],
	 uniformRule(naturalization)).
usedFor([taxes, duties, imposts, excises],
	 uniformRule(laws(bankruptcies(throughoutUS)))).
usedFor([taxes, duties, imposts, excises],coin(money)).
usedFor([taxes, duties, imposts, excises],regulate(value)).
usedFor([taxes, duties, imposts, excises],regulate(foreignCoin)).
usedFor([taxes, duties, imposts, excises], fix(standardofWeights_Measures)).

usedFor([taxes, duties, imposts, excises], punishment(counterfeiting(securities))).
usedFor([taxes, duties, imposts, excises], punishment(counterfeiting(currentCoinUS))).

usedFor([taxes, duties, imposts, excises], establish(postOffices)).
usedFor([taxes, duties, imposts, excises], establish(postRoads)).

usedFor([taxes, duties, imposts, excises], promote(scienceAndArts)).
usedFor([taxes, duties, imposts, excises],  promote(securing(limitedTimes(authorsAndInventors(
exclusiveRight(theirWritingsAndDiscoveries)))))).

usedFor([taxes, duties, imposts, excises], constitute(tribunals(inferior(supremeCourt)))).

usedFor([taxes, duties, imposts, excises], defineAndPunish(piraciesAndFelonies(committed(highSeas)))).

usedFor([taxes, duties, imposts, excises], defineAndPunish(piraciesAndFelonies(committed(offencesAgainstLawOfUS)))).

usedFor([taxes, duties, imposts, excises], declare(war)).

usedFor([taxes, duties, imposts, excises], grant(letters(marqueAndReprisal))).

usedFor([taxes, duties, imposts, excises], makingRules(capturedLandAndWater)).

usedFor([taxes, duties, imposts, excises], raiseAndSupport(armiesButNotMoreThan2Years)).

usedFor([taxes, duties, imposts, excises],provideAndMaintain(navy)).

usedFor([taxes, duties, imposts, excises], makeRules(government)).

usedFor([taxes, duties, imposts, excises], makeRegulation(landAndNavalForces)).

usedFor([taxes, duties, imposts, excises], executing(laws)).

usedFor([taxes, duties, imposts, excises], suppress(insurrectionsAndRepelInvasions)).

usedFor([taxes, duties, imposts, excises], organising(arming(disciplining(militia)))).

usedFor([taxes, duties, imposts, excises], governing(employeesOfUS)).

usedFor([taxes, duties, imposts, excises],reserving(states)).

usedFor([taxes, duties, imposts, excises], appointment(officers)).

usedFor([taxes, duties, imposts, excises], training(militia(disciplinePrescribedByCongress))).

usedFor([taxes, duties, imposts, excises], exercise(exclusiveLegislation(allCases(district(notExceeding(tenMilesSquare)))))).
usedFor([taxes, duties, imposts, excises], exercise(exclusiveLegislation(cession(particularStates(acceptanceOfCongress(become(seatOfGovernmentUS(exercise(authority(placesPurchased(consentOf(legislatureState(erection([forts, magazines, arsenals, dock_Yards , needfulBuildings])))))))))))))).


usedFor([taxes, duties, imposts, excises], make(allLaws(carrying(executing(foregoingPowers))))).
usedFor([taxes, duties, imposts, excises], make(allLaws(carrying(powersVested(constitution(governmentUS(departmentOrOfficerany))))))).

% section 9

restriction(congress , prior(year_oneThousandEightHundredAndEight(taxOrDuty(imposed(importation(notExceeding(ten_dollars(perPerson)))))))).

restriction( congress, privilege(writ(habeasCorpus(notSuspended(unless(inCasesOf(rebellionOrInvasion(publicSafety)))))))).

restriction( congress , noTaxOrDuty(laid(articles(exported(anyState))))).

restriction( congress , noPreference(anyRegulation(commerceOrRevenue(ports(oneStateOverAnother(norVessels(obliged(enter(clearOrPayDuties(anotherState)))))))))).

restriction( congress , noMoney(drawn(treasury(butinConsequence(appropriations(law(regular(statementAndAccount(receiptsAndExpenditures(publicMoney(published ))))))))))).


restriction( congress , noTitle(nobility(unitedStates))).
restriction( congress , noPerson(holding(office(withoutTheConsent(congress(accept(present([emolument, office, title, king, prince, orforeignState])))))))).

% section 10

restriction( state , notAllowed(enter(treaty))).

restriction( state , notAllowed(enter(alliance))).

restriction( state , notAllowed(enter(confederation))).

restriction( state , notAllowed(grant(letters(marqueAndReprisal)))).

restriction( state , notAllowed(coin(money))).

restriction( state , notAllowed(emit(credit(bills)))).

restriction( state , notAllowed(make(anyThingBut(goldAndSilverCoin(tender(payment(debts))))))).

restriction( state , notAllowed(pass(bill(attainder)))).

restriction( state , notAllowed(pass(exPostFactoLaw))).

restriction( state , notAllowed(pass(law(impairing(obligation(contracts)))))).

restriction( state , notAllowed(grant(titleOfNobility))).

restrictedWithoutTheConsentOfCongressTo( state , notAllowed(lay(impostsOrDuties(importsOrExports(except(executing(inspection(laws)))))))).

restrictedWithoutTheConsentOfCongressTo( state , notAllowed(lay(impostsOrDuties(netProduce(dutiesAndImposts(use(treasury(theUS(suchLaws(subject(revisionAndControl(congress)))))))))))).

restrictedWithoutTheConsentOfCongressTo( state , notAllowed(lay(duty(ofTonnage(keepTroopsOrShipsOfWar(timeOfPeace)))))).

restrictedWithoutTheConsentOfCongressTo( state , notAllowed(enter(agreementOrCompact(anotherState(foreignPower))))).

restrictedWithoutTheConsentOfCongressTo( state , notAllowed(engage(warUnlessInvaded(imminentDanger(notAdmit(delay)))))).


% Article 2

% Article 2 :  Section 1 :

executivePower(president).
power(president, executivePower).

term(president, years(4)).
term(vicePresident, years(4)).
sameTerm(president, vicePresident).

conditionElectors([not(senator), not(represenatative), not(person(holding(officeOfTrustOrProfit)))]).
appoint(S, electors(S, condition(X))) :-  stateOfUS(S), conditionElectors(X).
numElectors(Senators, Representatives, Electors) :- Electors is Senators + Representatives.

meet(electorsOf(S), state(S)) :- stateOfUS(S).

% Statements before Amendment XII
% vote(electorsOf(S), byBallot(persons(2, condition(atleast1(not(inhabitant(S))))))) :- stateOfUS(S).
% duties(of(electors), makeList(names(persons(votedFor)))).
% duties(of(electors), makeList(numVotes(votedPerson))).

% Changes due to Amendment XII
vote(electorsOf(S), byBallot(president, vicePresident, condition(president(not(inhabitant(S)))))) :- stateOfUS(S).
vote(electorsOf(S), byBallot(president, vicePresident, condition(vicePresident(not(inhabitant(S)))))) :- stateOfUS(S).
vote(electorsOf(S), byBallot(president, vicePresident, condition(presidentAndVicePresident(not(inhabitant(S)))))) :- stateOfUS(S).
duties(of(electors), makeList(names(persons(votedFor(president))))).
duties(of(electors), makeList(names(persons(votedFor(vicePresident))))).
distinct(list(names(persons(votedFor(president)))), list(names(persons(votedFor(vicePresident))))).
% End of Changes due to Amendment XII

duties(of(electors), sign(certify(list))).
duties(of(electors), transmit(to(seat(govtUS)), directedTo(president(senate)))).

duties(of(president(senate)), open(certificates(inPresenceOf(senate, houseOfRepresentatives)))).
duties(of(president(senate)), ensure(count(votes(inPresenceOf(senate, houseOfRepresentatives))))).

% Statements before Amendment XII
% president(person(max(votes), condition(equal(max(votes), majority(numAppointed(electors)))))).
% president(person(chosenBy(houseOfRepresentatives(fromBallot)), condition(majority(numAppointed(electors(moreThanOne), equal(numVotes(majority))))))).
% president(person(chosenBy(houseOfRepresentatives(from(highest5OnList))), condition(no(majority), votes(taken(by(states), representation(eachState(oneVote))))))).

% Changes due to Amendment XII
president(person(max(votes(votedFor(president))), condition(equal(max(votes(votedFor(president))), majority(numAppointed(electors)))))).
president(person(chosenBy(houseOfRepresentatives(from(highest3OnList))), condition(no(majority), votes(taken(by(states), representation(eachState(oneVote))))))).
president(vicePresident, condition(houseOfRepresentatives(notChoose(president, until(nextFollowing(fourthDayOfMarch)))))).
% End of Changes due to Amendment XII

quorumMember(member(S)) :- stateOfUS(S).
quorumMember(members(twoThirds(states))).
necessaryChoice(majorityOfAllStates).

% Statements before Amendment XII
% vicePresident(person(max(votes), condition(afterElecting(president)))).
% vicePresident(person(chosenBy(senate(fromBallot)), condition(afterElecting(president), equalVotes(twoOrMorePersons)))).

% Changes due to Amendment XII
vicePresident(person(max(votes(votedFor(vicePresident))), condition(equal(max(votes(votedFor(vicePresident))), majority(numAppointed(electors)))))).
vicePresident(person(chosenBy(senate(from(highest2OnList))), condition(no(majority), votes(taken(by(states), representation(eachState(oneVote))))))).
% End of changes due to Amendment XII

power(congress, determine(time(choosing(electors)))).
power(congress, determine(day(giving(votes)))).
day(giving(votes(sameFor(X, Y)))) :- stateOfUS(X), stateOfUS(Y).

conditionEligible(mustBe(citizen(naturalBorn))).
conditionEligible(mustBe(citizen(us))).
conditionEligible(mustHave(age(greaterThanEqualTo(years(35))), residentWithinUS(greaterThanEqualTo(years(14))))).
eligible(for(office(president)), condition(X, during(adoption(constitution)))) :- conditionEligible(X).

casePowerHandoff(removeFromOffice(president)).
casePowerHandoff(death(president)).
casePowerHandoff(resignation(president)).
casePowerHandoff(inability(discharge(powers(president)))).
casePowerHandoff(inability(discharge(duties(president)))).
devolve(power(of(president), to(vicePresident), case(X))) :- casePowerHandoff(X).

% Statements due to Amendment XX :  Section 3 :
caseNewPowerHandoff1(death(president(before(begin(term))))).
caseNewPowerHandoff2(notChosen(president(as(begin(term))))).
caseNewPowerHandoff2(failedToQualify(president)).
devolve(power(of(president), to(vicePresident), case(X))) :- caseNewPowerHandoff1(X).
devolve(power(of(president), to(vicePresident), case(X, condition(until(qualified(president)))))) :- caseNewPowerHandoff2(X).
% End of statements due to Amendment XX :  Section 3 :

% Statements before Amendment XX : Section 3 :
% power(congress, declare(officer(president), case(X), condition(post(president(until(removal(disability), re(elect(president)))))))) :- casePowerHandoff(X).

% Changes due to Amendment XX :  Section 3 :
power(congress, declare(president, case(failedToQualify(president, vicePresident), until(qualified(president, vicePresident))))).
power(congress, declare(manner(elect(president)), case(failedToQualify(president, vicePresident), until(qualified(president, vicePresident))))).

compensation(to(president), for(services(noIncrementOrDecrement(till(electedPeriod))), condition(notReceive(thatPeriod(otherEmoulumentfromUS))))).
duties(of(president), before(executionOfOffice(takeOath("I do solemnly swear (or affirm) that I will faithfully execute the Office of President of the United States, and will to the best of my Ability, preserve, protect and defend the Constitution of the United States.")))).

% Article 2 : Section 2 :

commanderInChiefOfArmy(president(case(actualService))).
commanderInChiefOfNavy(president(case(actualService))).
commanderInChiefOfMilitiaOfState(S, president(case(actualService))) :- stateOfUS(S).

power(president, commanderInChiefOfArmy(case(actualService))).
power(president, commanderInChiefOfNavy(case(actualService))).
power(president, commanderInChiefOfMilitiaOfState(case(actualService(S)))) :- stateOfUS(S).
power(president, grant(pardon(offenseAgainstUS))).
power(president, grant(reprieve(offenseAgainstUS))).

require(president, writing(principalOfficer(executiveDepartment(duties(offices))))).

conditionTreaties(concur(twoThirds(senators))).
conditionSenate(advice(consent(senate))).

power(president, makeTreaties(condition(X))) :- conditionTreaties(X).
power(president, makeTreaties(condition(X))) :- conditionSenate(X).
power(president, nominate(appoint(ambassadors(condition(X))))) :- conditionSenate(X).
power(president, nominate(appoint(publicMinisters(condition(X))))) :- conditionSenate(X).
power(president, nominate(appoint(consuls(condition(X))))) :- conditionSenate(X).
power(president, nominate(appoint(judges(supremeCourt((condition(X))))))) :- conditionSenate(X).
power(president, nominate(appoint(otherOfficers(condition(X))))) :- conditionSenate(X).
power(president, appoint(inferiorOfficers)).
power(courtsOfLaw, appoint(inferiorOfficers)).
power(headsOfDepartment, appoint(inferiorOfficers)).
power(president, grantCommissions(fill(vacancies(case(recessOfSenate))))).

% Article 2  : Section 3 :

house(houseOfRepresentatives, senate).

duties(of(president), info(congress(about(stateOfUnion)))).
duties(of(president), recommend(congress(necessary(expedient(measures))))).
duties(of(president), convene(houseOfRepresentativesAndSenate(case(extraordinaryOcassions)))).
duties(of(president), convene(houseOfRepresentatives(case(extraordinaryOcassions)))).
duties(of(president), convene(senate(case(extraordinaryOcassions)))).
duties(of(president), adjourn(houseOfRepresentatives(case(disagreement(X, Y))))) :- house(X, Y).
duties(of(president), adjourn(senate(case(disagreement(X, Y))))) :- house(X, Y).
duties(of(president), receive(ambassadors)).
duties(of(president), receive(otherPublicMinisters)).
duties(of(president), takeCare(laws(faithfullyExecuted))).
duties(of(president), commission(officers(us))).

% Article 2 : Section 4 :

caseRemoval(impeachment(conviciton(treason))).
caseRemoval(impeachment(conviciton(bribery))).
caseRemoval(impeachment(conviciton(highCrimes))).
caseRemoval(impeachment(conviciton(misdimeanors))).

removeFromOffice(president, condition(X)) :- caseRemoval(X).
removeFromOffice(vicePresident, condition(X)) :- caseRemoval(X).
removeFromOffice(civilOfficers, condition(X)) :- caseRemoval(X).


% Article 3

% Article 3 : Section 1 :

judicialPower(supremeCourt).
judicialPower(inferiorCourts(ordain(establish(congress)))).
duties(of(judges(supremeCourt)), hold(office(case(goodBehaviour)))).
duties(of(judges(inferiorCourts)), hold(offices(case(goodBehaviour)))).
compensation(to(judges(supremeCourt)), for(services(until(continuance(office))))).
compensation(to(judges(inferiorCourts)), for(services(until(continuance(office))))).

% Article 3 : Section 2 :

conditionJudicialPower(lawAndEquity(constitution)).
conditionJudicialPower(laws(us)).
conditionJudicialPower(treaties(authority(us))).
conditionJudicialPower(affecting(ambassadors)).
conditionJudicialPower(affecting(publicMinisters)).
conditionJudicialPower(affecting(consuls)).
conditionJudicialPower(admiralty).
conditionJudicialPower(maritimeJurisdiction).
conditionJudicialPower(controversies(party(us))).
conditionJudicialPower(controversies(party(us))).
conditionJudicialPower(controversies(states(twoOrMore))).
conditionJudicialPower(controversies(states, citizens(differentState))).
conditionJudicialPower(controversies(citizens(differentStates))).
conditionJudicialPower(controversies(citizens(sameState), claim(land(grants(differentStates))))).
conditionJudicialPower(controversies(states, citizens(anotherState))).
conditionJudicialPower(controversies(states, foreign(states, citizens, subjects))).
conditionJudicialPower(controversies(citizens, foreign(states, citizens, subjects))).

extensionJudicialPower(case(X)) :- conditionJudicialPower(X).

caseOriginalJurisdiction(affecting(ambassadors)).
caseOriginalJurisdiction(affecting(publicMinisters)).
caseOriginalJurisdiction(affecting(consuls)).
caseOriginalJurisdiction(party(S)) :- stateOfUS(S).

originalJurisdiction(supremeCourt, case(X)) :- caseOriginalJurisdiction(X).
appellateJurisdiction(supremeCourt, case(not(X))) :- caseOriginalJurisdiction(X).

trailOfCrimes(jury, conditon(not(impeachment))).
placeOfTrial(crimeCommitted(S), S) :- stateOfUS(S).
placeOfTrial(crimeCommitted(not(stateOfUS)), places(directedbyLaw(congress))).


% Article 3 : Section 3 :

conditionTreason(levyWar(given(testimony(witnesses(twoOrMore))))).
conditionTreason(levyWar(given(confession(openCourt)))).
conditionTreason(adhereToEnemies(given(testimony(witnesses(twoOrMore))))).
conditionTreason(adhereToEnemies(given(confession(openCourt)))).
conditionTreason(aidAndSupportEnemies(given(testimony(witnesses(twoOrMore))))).
conditionTreason(aidAndSupportEnemies(given(confession(openCourt)))).

treasonAgainstUS(case(X)) :- conditionTreason(X).

conditionPunishment(attainderOfTreason(not(work(corruptionOfBlood(except(during(attainedLife))))))).
conditionPunishment(attainderOfTreason(not(forfeiture(except(during(attainedLife)))))).

power(congress, declare(treasonPunishment(condition(X)))) :- conditionPunishment(X).


% Article 4
% describes some of the powers of congress, nature of rules, citizen
% rights, privileges & protective laws offered to the states

% Section1
fullFaithAndCredit(public_acts).
fullFaithAndCredit(records).
fullFaithAndCredit(judicial_proceedings).
power(congress, general_laws(prescribe_manner(prove(public_acts)))).
power(congress, general_laws(prescribe_manner(prove(records)))).
power(congress, general_laws(prescribe_manner(prove(judicial_proceedings)))).


% Section 2
citizensEntitledTo(privileges).
citizensEntitledTo(immunities).
returnFugitive((chargedWith(treason), chargedWith(felony), chargedWith(crime))).
% Changes/Additions due to Amendment 13
should_not_exist(slavery, involuntary_servitude).
punishment_for_crime(slavery_or_involuntary_servitude, duly_convicted).


% Section 3
power(congress, admit_into_union(new_states)).
power(congress, dispose(rules)).
power(congress, make(rules)).
no_such_rule_be_constructed(prejudice(claims(united_states))).
no_such_rule_be_constructed(prejudice(claims(state))).


% Section 4
republicanGovernment(new_hampshire).
republicanGovernment(massachusetts).
republicanGovernment(connecticut).
republicanGovernment(new_york).
republicanGovernment(new_jersey).
republicanGovernment(pennsylvania).
republicanGovernment(delware).
republicanGovernment(maryland).
republicanGovernment(virginia).
republicanGovernment(north_carolina).
republicanGovernment(south_carolina).
republicanGovernment(georgia).

protectionAgainstInvasion(new_hampshire).
protectionAgainstInvasion(massachusetts).
protectionAgainstInvasion(connecticut).
protectionAgainstInvasion(new_york).
protectionAgainstInvasion(new_jersey).
protectionAgainstInvasion(pennsylvania).
protectionAgainstInvasion(delware).
protectionAgainstInvasion(maryland).
protectionAgainstInvasion(virginia).
protectionAgainstInvasion(north_carolina).
protectionAgainstInvasion(south_carolina).
protectionAgainstInvasion(georgia).

protectionAgainstDomesticViolence(new_hampshire).
protectionAgainstDomesticViolence(massachusetts).
protectionAgainstDomesticViolence(connecticut).
protectionAgainstDomesticViolence(new_york).
protectionAgainstDomesticViolence(new_jersey).
protectionAgainstDomesticViolence(pennsylvania).
protectionAgainstDomesticViolence(delware).
protectionAgainstDomesticViolence(maryland).
protectionAgainstDomesticViolence(virginia).
protectionAgainstDomesticViolence(north_carolina).
protectionAgainstDomesticViolence(south_carolina).
protectionAgainstDomesticViolence(georgia).


% Article 5
% Describe the power of congress to propose the amendments to the
% constitution
power(congress, amendments_to_constitution(deemed_necessary(two_thirds(both_houses)), ratified(legislatures(three_fourths(states))))).
no_state_without_consent_shall_be_deprived_of(equal_suffrage(senate)).

% Article 6
% Describes the members liable to support the constitution
debts_engagements(valid_under_US, valid_under_confederation).

supreme_law_of_the_land(constitution).
supreme_law_of_the_land(laws(united_states)).
supreme_law_of_the_land(authority_United_States(treaties)).

bound_by_oath_to_support_the_constitution(senators).
bound_by_oath_to_support_the_constitution(representatives).
bound_by_oath_to_support_the_constitution(members(state_legislatures)).
bound_by_oath_to_support_the_constitution(executive_officers).
bound_by_oath_to_support_the_constitution(judicial_officers).
no_religious_test_required(qualification(office)).
no_religious_test_required(qualification(public_trust)).


% Article 7
% Consists of the names of the representatives from various states
date_of_convention(17, 9, 1787).
day_of_convention(monday).
president(go_Washington).
deputy_Virginia(go_Washington).
representative(new_hampshire, john_Langdon).
representative(new_hampshire, nicholas_Gilman).
representative(massachusetts, nathaniel_Gorham).
representative(massachusetts, rufus_King).
representative(connecticut, wm_Saml_Johnson).
representative(connecticut, roger_Sherman).
representative(new_york, alexander_Hamilton).
representative(new_jersey, wil_Livingston).
representative(new_jersey, david_Brearley).
representative(new_jersey, wm_Paterson).
representative(new_jersey, jona_Dayton).
representative(pennsylvania, b_Franklin).
representative(pennsylvania, thomas_Mifflin).
representative(pennsylvania, robt_Morris).
representative(pennsylvania, geo_Clymer).
representative(pennsylvania, thos_FitzSimons).
representative(pennsylvania, jared_Ingersoll).
representative(pennsylvania, james_Wilson).
representative(pennsylvania, gouv_Morris).
representative(delaware, geo_Read).
representative(delaware, gunning_Bedford_jun).
representative(delaware, john_Dickinson).
representative(delaware, richard_Bassett).
representative(delaware, jaco_Broom).
representative(maryland, james_McHenry).
representative(maryland, dan_of_St_Thos_Jenifer).
representative(maryland, danl_Carroll).
representative(virginia, john_Blair).
representative(virginia, james_Madison_Jr).
representative(north_carolina, wm_Blount).
representative(north_carolina, richd_Dobbs_Spaight).
representative(north_carolina, hu_Williamson).
representative(south_carolina, j_Rutledge).
representative(south_carolina, charles_Cotesworth_Pinckney).
representative(south_carolina, charles_Pinckney).
representative(south_carolina, pierce_Butler).
representative(georgia, william_Few).
representative(georgia, abr_Baldwin).

%Amendments 1-10

right(X,Y) :- citizen(X,_) ,
	Y = noLaw(respecting(oneReligion));
	Y = prohibiting(freeExercise) ;

	Y = abridging(freedomOfSpeech);
	Y = abridging(freedomOfPress);

	Y = right(people(assemble(petition(government(grievances)))));

Y = right(people(keepAndBear(arms(notBeInfringed))));

Y = noSoldier(inTimeOfPeace(quartered(anyHouse(withoutConsent(owner(norDuring(war(butManner(prescribedByLaw)))))))));

Y = right(people(secure(persons(houses(papers(effects(againstUnreasonable(searchesAndSeizures(noWarrantsIssued(causeSupported(oathOrAffirmation(place(searched(personsOrthings(seized)))))))))))))));

Y = noPerson(answer(capitalOrInfamous(crime(unless(presentmentOrIndictment(grandJury(except(casesIn(land(naval_forces(militia(inTime(war(public_danger(sameOffence(notTwice(jeopardyOf(life(limb(notCompelled(criminalCase(witnessAgainstHimself(notDeprived(life(liberty(property(withoutProcess(law(noPrivateProperty(takenForPublicUse(without(compensation)))))))))))))))))))))))));

Y = right(speedyAndPublicTrial(impartialJury(stateAndDistrict(whereinCrimeCommitted(districtAscertainedByLaw(informed(natureAndCause(accusation(confronted(witnessesAgainstHim(compulsoryProcess(obtainingWitnesses(favour(assistanceOfCounsel(defence)))))))))))))));

Y = right(trial(jury));
Y = noFact(tried(jury(reexamined(anyCourt(theUS)))));


Y = excessive(bail(notRequired));
Y = noExcessive(finesImposed);
Y = noCruelAndUnusual(punishments(inflicted));

Y = enumeration(constitution(notConstrued(denyOrDisparage(othersRetained(people)))));

Y = powers(notDelegated(theUS(constitution(norProhibited(theStates(reserved(states)))))));

Y = powers(notDelegated(theUS(constitution(norProhibited(theStates(reserved(people)))))));

Y = vote(inElectionOf([president, vice_president,  electorsFor_President, electorsFor_VicePresident, senator , representativeInCongress],notDenied(theUS(failure(payTax))))));

Y = vote(inElectionOf([president, vice_president,  electorsFor_President, electorsFor_VicePresident, senator , representativeInCongress]),notDenied(anyState(failure(payTax)))))))))).

%Amendment 11
%Modified in Article 3 Section 2

%Amendment 12
%modified in Article 2 Section 1

%Amendment 13
%Included in Section 2 of Article 4

%Amendment 14

passed(amendment_14,july_13_1866).
ratified(amendment_14,july_09_1868).

% section 1

citizenOfState(X,personBorn):-stateOfUS(X).
citizenOfState(X,personNaturalized):-stateOfUS(X).

restriction(state,cannotMake(lawTo(abridge(privileges(citizensOfUS))))).
deprive(life).
deprive(liberty).
deprive(property).
restriction(state,cannotDeprive(person((withoutProcessOfLaw(X))))):-deprive(X).
restriction(state,cannotDeny(person(protectionOfLaw))).

% For section 2 statements necessary changes made in Article 1 section 2

% section 3

restriction(citizen,holdOffice(ofUS(previouslyEngagedIn(rebellion)))).
restriction(citizen,holdOffice(ofUS(previouslyEngagedIn(aidingEnemies)))).
power(congress,vote(two_thirdsTo(removeDisabilty(citizen(toHold(office)))))).

% section 4

notQuestioned(validity(debtIncurred(pensions))).
notQuestioned(validity(debtIncurred(bountiesFor(supressing(rebellion))))).
notPay(illegal(claim),debtIncurred(aidTo(rebellion))).
notPay(illegal(claim),debtIncurred(lossOf(slave))).

% section 5

power(congress,enforceBy(legislation(amendment_14))).

% Amendment 15 : Section 1 :

caseRightsDenial(race).
caseRightsDenial(color).
caseRightsDenial(prevCondition(servitude)).

noRightsDenial(of(usCitizens(onAccount(X))), by(us)) :- caseRightsDenial(X).
noRightsDenial(of(usCitizens(onAccount(X))), by(Y)) :- caseRightsDenial(X), stateOfUS(Y).


% Amendment 15 : Section 2 :

power(congress, enforce(byAppropriateLegislation(of(amendmentXV)))).


%Amendment 16

power(congress, layAndCollect(taxesOnIncomes(whateverSourceDerived(withoutApportionment(severalStates))))).

power(congress, layAndCollect(taxesOnIncomes(whateverSourceDerived(withoutRegard(censusOrEnumeration))))).

%Amendment 17

passed(amendment_17,may_13_1912).
ratified(amendment_17,april_08_1913).

% necessary changes made in Article 1, section 3

notAffect(senator,term(chosenBefore(amendment_17(valid)))).

%Amendment 18
%repealed by Amendment 21
%Section 1

caseProhibited(manufacture(intoxicatingLiquors)).
caseProhibited(sale(intoxicatingLiquors)).
caseProhibited(transportation(intoxicatingLiquors)).
caseProhibited(importation(intoxicatingLiquors)).
caseProhibited(exportation(intoxicatingLiquors)).

prohibited(yearFromAmendmentXVIII(1), X) :- caseProhibited(X).

% Amendment 18 : Section 2 :

power(congress, enforce(byApropriateLegislation(of(amendmentXVIII)))).
power(S, enforce(byAppropriateLegislation(of(amendmentXVIII)))) :- stateOfUS(S).

%Amendment 19
right_should_not_be_denied_on_account_of_sex(vote).
power(congress, enforce(amendments(article19))).

%Amendment 20
%Modified Article 2

%Amendment 21
% Section 1
repealed(amendments(article18)).

% Section 2
prohibited(transportation_or_importation('intoxicating liquors')).

% Section 3
operative_after_ratification(amendments(article21)).

% Amendment 22

% Section 1
not_eligible(office(president), elected_twice_before).
eligible_once(office(president), held(office(president), more_than_two_years(term(other_person(elected(president)))))).
not_applicable(amendments(article22), person(office(president), article_proposed(congress))).
shall_not_prevent_from_holding_the_office_of_president(amendments(article22), person(office(president), term(operative(amendments(article22))))).

% Section 2
operative_after_ratification(amendments(article22)).

% Amendment 23

% Section 1
appointment(district(seat(government)), 'A number of electors of President and Vice President equal to the whole number of Senators and Representatives in Congress to which the District would be entitled if it were a State, but in no event more than the least populous State; they shall be in addition to those appointed by the States, but they shall be considered, for the purposes of the election of President and Vice President, to be electors appointed by a State; and they shall meet in the District and perform such duties as provided by the twelfth article of amendment.').

% Section 2
power(congress, enforce(amendments(article23))).

%Amendment 24
%Section 1 already covered with right predicates.
%Section 2
power( congress , enforce(rightToVote(appropriateLegislation))).

%Amendment 25
%Modification made in Article 2 Section 1.

%Amendment 26

passed(amendment_26,march_23_1971).
ratified(amendment_26,july_01_1971).

/*

changes for section 1 made in Article 1 section 2 since Amendment 14
section 2 changes are reflected there

*/

% section 2
power(congress,enforceBy(legislation(amendment_26))).

%Amendment 27
noLaw(senate, houseOfRepresentative,noVarying(compensation(servicetakeEffect(untilElection( representatives))))).


% ----------------------------------END--------------------------------




















