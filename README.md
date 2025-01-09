# directed-smart-contract
Plutus scripts for DirectEd Smart Contract.

* In the [.devcontainer](.devcontainer/) directory, you'll find a Docker contained created by the IOG Education Team to create a local plutus development, made for the Plutus Pioneers Program Iteration 4.

# Abstract
In the US, UK, and Europe alone, more than $160 billion is distributed in grants to NGOs every year, with billions more given out worldwide under varied levels of oversight. Many public grant-making processes remain opaque, centralized, and prone to corruption, leading to significant misuse of funds, as exemplified by Kenya's $1.4 billion loss in 2019. Our open-sourced Cardano-based framework addresses these systemic challenges by offering real-time, auditable transparency through Plutus-powered smart contracts. This system clearly defines each stakeholder role, including Facilitators, Approvers, and Examiners, distributing decision-making authority across multiple agents. The framework automates milestone-based funding, releasing conditional tranches only upon verified completion of agreed-upon goals. By distinguishing roles and encoding approvals with NFTs, it significantly reduces the risk of corruption and lowers operational overhead. This approach offers a fairer, more efficient, and accountable model for distributing and tracking public funds across diverse settings. Through checks, it ensures fund allocation.

## Background
In US, UK and Europe alone, more than $160 billion is distributed in grants to NGOs every year. Billions more are given out in other countries, often with much less rigorous and fair processes. For example, it was estimated that $1.4 billion worth of public funds in Kenya were stolen in 2019 alone. 

Regardless of the country or severity of the problem, all public grant-making funds share a few challenges. Firstly, there's a lack of transparency - both in terms of who is behind the decisions, how the decisions are made and the flow of funds. Secondly, there is centralisation of decision-making and power, increasing the risk of corruption. Thirdly, a trade-off between either a lack of accountability and oversight OR very high operational overhead. 

## General-Purpose Smart Contract Framework
We present an open-sourced framework build on Cardano using Plutus to address the lack of real-time auditable transparency in the use of funds, centralised control of such funds and operational inefficiency. 

### Presentation links
- Slides: https://pitch.com/v/directed-smart-contract-grant-system-4ehpxu
- Video: https://youtu.be/vPsOWtTO6Qo

#### Transparency 
Any transaction on a blockchain is public and independently verifiable in real time. Our framework offers meaningful transparency in addition to this by clearly defining each stakeholder who share the control over the flow of funds. This is presented in detail later. 

#### Centralisation
We solve the issue of centralisation of power by delegating different levels of control to different personas (roles). This is qualitatively different from utilising multi-signature wallet (multi-sigs) as the latter has full authority and freedom to move funds as they please. In contrast, our framework allows users to distribute (a) the decision on the grant, loan or investment recipient, (b) the power on whether a milestone has been passed or not, (c) the distribution of the funds, *assuming that (a) and (b) have been decided and done*. 

#### Operationally 
We reduce costs by using smart contracts which automates the distribution of funds. Specifically, a tranche of funds can only be released conditional on (a) being the true Recipient of funds, and (b) having met the milestones. Both of these criteria are encoded in the smart contracts (more details below). 

### Definitions
We present the definitions in the most pedagogical order (chronological from a process point of view) instead of alphabetical. 

"Agent": an Agent is defined by defined by its responsibilities and powers vis-a-vis the funds that are to be distributed. In practice, a real person or a single wallet (defined as a staking private key in the case of Cardano) can be several types of "Agents" at the same time. There are 5 types of Agents.

"Facilitator": the Agent (single) defining the parameters of the milestones smart contract, responsible for deployment, defines which people (Cardano wallets) will be Reviewers and Examiners.
"Investor": the Agent(s) providing liquidity to the pooled smart contract(s). 
"Approver": the Agent(s) with the (joint, if multiple) power to approve which "Recipient(s)" will receive Funding. 
"Recipient": the Agent(s) receiving the funds. 
"Examiner": the Agent(s) with the power to determine if a milestone has been reached. A real person can control an Examiner Agent and use its power. A smart contract can also serve as the Examiner. The latter would require an Oracle to provide it with data to and from a blockchain to other digital systems outside the blockchain. 

"Pooling Smart Contract" (PSC): this contract is parameterised by the Facilitator and defines the stake key(s) of the Approver(s) and Examiner(s). In future versions, this contract may also contain expiry dates such that if milestones have not been met by that date, funds from the Milestones Smart Contract can be returned to the Pooling Smart Contract, and then to the Investor. 

"Milestones Smart Contract (MSC)": defines the Recipients, number of milestones, token amount per tranch of funds released upon completion of each milestone, and the amount of each token that is included in each tranch.

![DirectEd Smart Contract Funding Framework](https://github.com/user-attachments/assets/8e34dd4a-7271-4f30-8cd2-dc777c10d99d)

### How does it work? 
Let F, A, B, N and M be integer variables strictly greater than 0. 


1. Facilitator parameterizes the Pooling Smart Contract and defines the Approver(s) and Examiner(s) by the stake addresses on the Cardano blockchain.
2. Investors send funds in the form of fungible tokens to the "Pooling Smart Contract" on the Cardano blockchain. A mix of tokens, both ADA and native tokens, can be sent. 
3. Approvers send an Approver NFT to the wallet of "Recipient" on Cardano, one each if there are multiple. This means that they approve Recipient for a distribution for a total amount (henceforth denoted "F") divided into a number of tranches/milestones (henceforth denoted "M"), each with an amount (henceforth "A"). Thus, F=M*A is the expression that defines the total funding amount approved for "Recipient". Henceforth, "Approving" refers to the act of sending an "Approver NFT" to the wallet of whoever is approved for a funding amount.
4. "Recipient" sends Approver NFTs to the "Pooling Smart Contract". This initialises the "Milestones Smart Contract", taking F from "Pooling Smart Contract" to "Milestones Smart Contract". Each Recipient has a unique "Milestones Smart Contract".
5. The "Milestones Smart Contract" is created as part of the process in step 4. 
6. Examiner(s) mints and sends a "Milestone NFT" for completion milestone M to "Recipient" upon presentation of satisfactory completion of milestone M. 
7. "Recipient" presents (in a transaction) the Milestone NFT(s) required to receive the tranch A that is tied to the completion of milestone M to the "Milestones Smart Contract".
8. The smart contract burns the milestone NFTs and returns "A" amount to "Recipient".

The process is repeated until all milestones have been met, or if the Deadline defined in the "Milestones Smart Contract" has been reached. The latter case, funds are returned to a predefined address.

Extra notes 
- Amount A and types of Cardano native tokens can vary for each milestone M. 
- Each milestone distribution require a unique set of NFTs. 
- The NFT from each Reviewer is necessary but not sufficient to receive milestones funding (Examiner NFT is also necessary). 
- The Pooling Smart Contract contains a timer such that unused funds in the Milestones Smart Contract can be returned to a pre-defined wallet. This is a transaction that needs to be triggered through the requisite transaction and signature(s).


### Overview of Scholarship Use-case of the DirectEd Funding Framework:
The smart contract manages scholarships in such a way that for a student to withdraw portions of the scholarship, they must be approved by three different institutions (these are all "Approvers" in the terminology defined in the previous section) - the ‘Authority’, the ‘School’, and the ‘CourseProvider’. These institutions register their approval of a particular scholar by issuing a certain token to the scholar, which the smart contract verifies before allowing withdrawals. Note that a single instance of the smart contract allows for scholarships between a single Authority, a single School, and a single CourseProvider.

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
8. If scholarship students fail to satisfy some of the conditions within the pre-specified time frame written in the smart contract, whatever funds remain can be redirected to a wallet pre-specified by DirectEd. 

### Technical Workflow:

1. A scholarship is specified by choosing the *PoolParams*. This includes choosing:
    1. *pAuthority* - which corresponds to the primary scholarship giving authority (i.e. DirectEd), who 
    2. *pSchool* - which corresponds to the high school the students come from
    3. *pCourseProvider* - which corresponds to the institution in charge of the course
    4. *pAmount* - the total amount (in lovelace) that each scholarship is worth
    5. *pMilestones* - the number of milestones in the course of education. Each time the student completes a milestone, they will be able to withdraw *pAmount*/*pMilestones* ADA from their scholarship. 
    6. *pDeadline* - the deadline for completion of the course. (Currently Unused)
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
    3. It must validate before the *pDeadline* (Note: Currently unused)
    4. It must withdraw *pAmount/pMilestones* lovelace and send the rest back to the scholarship script with the *Milestone* number increased by 1. (Or if we are on the last milestone, send nothing back to the script.)
7. In the event that a student has not completed their scholarship by *pDeadline* the scholarship giving authority *pAuthority* may reclaim any ADA that has not been withdrawn by students, and put this towards future scholarships. Note that having such a deadline is essential to ensure ADA cannot become permanently locked in the contract.(Note: Currently unused)
8. Until we are 100% confident in the security of these smart contracts, in the pool script and the scholarship script there is the option for *pAuthority* to reclaim any ADA sitting in the scripts. This is to be used in the event of a bug being discovered, so that the bug can be fixed and the ADA moved to the fixed smart contract.
