# directed-smart-contract
Plutus scripts for DirectEd Smart Contract.

### Overview:

The smart contract manages scholarships in such a way that for a student to withdraw portions of the scholarship, they must be approved by three different institutions - the ‘Authority’, the ‘School’, and the ‘CourseProvider’. These institutions register their approval of a particular scholar by issuing a certain token to the scholar, which the smart contract verifies before allowing withdrawals. Note that a single instance of the smart contract allows for scholarships between a single Authority, a single School, and a single CourseProvider.

Specifically, the ‘Authority’ should be the primary authority over the scholarship giving. When the authority issues a token to a student, this implies that they have selected that student for the scholarship out of all the candidate scholars. In our case the Authority will be DirectEd.

The ‘School’ is the high school that the student went to. Their token is issued to students from that school who have achieved above a certain threshold grade, guaranteeing both the identity of the scholarship recipient and their academic potential.

The ‘CourseProvider’ is the institution in charge of running the course that the scholarship is for. They issue tokens upon the student’s completion of various milestones, which the student can use to withdraw subsequent portions of their scholarship. 

**Non-technical Workflow**

1. DirectEd identifies high schools in low-income areas. 
2. DirectEd specifies a smart contract for each high school.
3. Donors pledge donation amounts to one of many smart contracts, each representing a high school.
4. High schools issue tokens to their students representing successful completion of their degree with a certain grade threshold. 
5. DirectEd issues tokens to students that are selected for the scholarship (i.e. make it through an application process)
6. Any student with a token from both their high school and DirectEd may show these to the smart contract in order to set aside a certain pot of money as their personal scholarship fund. 
7. Students then begin the course of education that the scholarship is designed for. Every time they successfully pass a milestone in this course, the course provider issues a token to them that represents this fact. The student can present this to the smart contract in order to withdraw a portion of their scholarship corresponding to that milestone. 
8. If scholarship students fail to satisfy some of the conditions within the pre-specified time frame written in the smart contract, whatever funds remain are returned to the donor addresses.

### Technical Workflow:

1. A scholarship is specified by choosing the *PoolParams*. This includes choosing:
    1. *pAuthority* - which corresponds to the primary scholarship giving authority (i.e. DirectEd), who 
    2. *pSchool* - which corresponds to the high school the students come from
    3. *pCourseProvider* - which corresponds to the institution in charge of the course
    4. *pAmount* - the total amount (in lovelace) that each scholarship is worth
    5. *pMilestones* - the number of milestones in the course of education. Each time the student completes a milestone, they will be able to withdraw *pAmount*/*pMilestones* ADA from their scholarship. 
    6. *pDeadline* - the deadline for completion of the course.
2. The *pAuthority*, *pSchool* and *pCourseProvider* found in *PoolParams* can be used in VerifiedByToken.hs to define three minting policies for the three institution’s tokens. (Note that the minting policies require the minted token to be sent to the *PaymentPubKeyHash* specified in the *TokenName -* the smart contract scripts will then double check that the signature of that *PaymentPubKeyHash* is present in any transaction attempting to use these tokens as evidence towards a scholarship.) These tokens will be referred to as:
    1. The acceptance token, issued by *pAuthority* to imply acceptance into the scholarship programme.
    2. The student status token, issued by *pSchool* to imply the student came from that high school and had the required grades.
    3. The milestone tokens, issued by *pCourseProvider* every time the student completes a milestone, to allow them to withdraw the next portion of their scholarship.
3. We combine the *PoolParams* with the three minting policies to define the *Scholarship*, which will serve as the parameters for the smart contract scripts.
4. The *Scholarship* can be used with ScholarshipPool.hs to create the first smart contract validator script, henceforth called the pool script. The *Scholarship* is also used with Scholarship.hs to create the second smart contract validator script, called the scholarship script. 
5. Donors will donate money by sending it directly to the pool script. A student from the correct school who has been selected for the scholarship will be able to initiate their own scholarship by creating a transaction that spends ADA from the pool script and sends it to the scholarship script with the correct datum. This transaction must satisfy the following conditions:
    1. It must burn an acceptance token and a student status token. (Note that we burn these tokens so that they cannot be reused by the student to create another scholarship for themselves). 
    2. It must withdraw exactly *pAmount* from the pool script. Specifically, the amount that it spends from the pool script minus the amount that it deposits back to the pool script must equal *pAmount* 
    3. It must create an output at the scholarship script with *pAmount* lovelace, and with the correct datum for a fresh scholarship for that student.
6. The scholarship script is designed as a state machine with the state being composed of the scholarship recipient’s *PaymentPubKeyHash* as well as the current *Milestone* The *Milestone* number is initialized at 0. Every time the recipient completes a milestone, and receives a milestone token from the *pCourseProvider* they can submit a transaction to the scholarship script that withdraws their next portion of scholarship funding. This transaction must satisfy the following conditions:
    1. It must burn a milestone token.
    2. It must be signed by the *PaymentPubKeyHash* listed in the state. 
    3. It must validate before the *pDeadline* 
    4. It must withdraw *pAmount/pMilestones* lovelace and send the rest back to the scholarship script with the *Milestone* number increased by 1. (Or if we are on the last milestone, send nothing back to the script.)
7. In the event that a student has not completed their scholarship by *pDeadline* the scholarship giving authority *pAuthority* may reclaim any ADA that has not been withdrawn by students, and put this towards future scholarships. Note that having such a deadline is essential to ensure ADA cannot become permanently locked in the contract.
8. Until we are 100% confident in the security of these smart contracts, in the pool script and the scholarship script there is the option for *pAuthority* to reclaim any ADA sitting in the scripts. This is to be used in the event of a bug being discovered, so that the bug can be fixed and the ADA moved to the fixed smart contract.
