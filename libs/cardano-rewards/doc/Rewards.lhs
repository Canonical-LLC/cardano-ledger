\documentclass[a4paper,11pt]{article}

%include polycode.fmt
%include spacing.fmt

\newcommand{\StakeCredential}{\type{StakeCredential}}
\newcommand{\StakeDistribution}{\type{StakeDistribution}}
\newcommand{\PoolID}{\type{PoolID}}
\newcommand{\Coin}{\type{Coin}}
\newcommand{\BlockCount}{\type{BlockCount}}
\newcommand{\N}{\ensuremath{\mathbb{N}}}
\newcommand{\Z}{\ensuremath{\mathbb{Z}}}
\newcommand{\Q}{\ensuremath{\mathbb{Q}}}
\newcommand{\unitInterval}{\ensuremath{[0,~1]}}
\newcommand{\PParams}{\type{ProtocolParameters}}
\newcommand{\PoolParameters}{\type{PoolParameters}}
\newcommand{\PoolRewardInfo}{\type{PoolRewardInfo}}

\usepackage[margin=2.5cm]{geometry}
\usepackage{lscape}
\usepackage{iohk}
\usepackage{microtype}
\usepackage{mathpazo} % nice fonts
\usepackage{amsmath}
\usepackage{amssymb}
\usepackage{amsthm}
\usepackage{latexsym}
\usepackage{mathtools}
\usepackage{stmaryrd}
\usepackage{extarrows}
\usepackage{slashed}
\usepackage[unicode=true,pdftex,pdfa,colorlinks=true]{hyperref}
\usepackage{xcolor}
\usepackage[capitalise,noabbrev,nameinlink]{cleveref}
\usepackage{float}
\floatstyle{boxed}
\restylefloat{figure}
\usepackage{tikz}
\usepackage{booktabs}
\usepackage{enumerate}
\usepackage{listings}
\usepackage{tocloft}
\addtolength{\cftsubsecnumwidth}{5pt}

\begin{document}

\title{The Cardano Reward Calculation.}
\date{}
\maketitle

\begin{abstract}
This document provides a standalone reference for the reward calculation of
the Cardano network, together with a reference implementation in haskell.
\end{abstract}

%if style == newcode
> {-# LANGUAGE DerivingVia #-}
>
>
> module Cardano.Rewards where
>
> import Data.Map (Map)
> import qualified Data.Map as Map
> import Data.Monoid (Sum (..))
> import Data.Maybe (fromMaybe)
> import Data.Ratio ((%))
> import Data.Set (Set)
> import qualified Data.Set as Set
>

%endif

\section{Overview}

TBD

\section{Types}

\begin{figure*}[htb]
  \emph{Abstract types}
  %
  \begin{equation*}
    \begin{array}{r@@{~\in~}lr}
      \var{cred} & \StakeCredential & \text{stake credential}\\
      \var{pid} & \PoolID & \text{stake pool ID}\\
    \end{array}
  \end{equation*}
  
  \emph{Derived types}
  %
  \begin{equation*}
    \begin{array}{r@@{\qquad=\qquad}lr}
      \Coin
      & \Z
      & \text{unit of value}
      \\
      \BlockCount
      & \Z
      & \text{a number of blocks}
    \end{array}
  \end{equation*}
  %
  \emph{Helper Functions}
  %
  \begin{equation*}
    \begin{array}{r@@{~::~}lr}
      \fun{coinFloor} & \Q \to \Coin & \text{convert a rational to a coin via floor}
      \\
      \fun{coinToQ} & \Coin \to \Q & \text{convert a coin to a rational}
      \\
      \fun{coinRatio} & \Coin \to \Coin \to \Q & \text{take a ratio of coins}
    \end{array}
  \end{equation*}
  
  \caption{Basic types and helper functions}
  \label{fig:defs:basic-types}
\end{figure*}

\begin{figure*}[htb]
  \emph{Protocol Parameters}
  \begin{equation*}
    \PParams =
    \left(
      \begin{array}{r@@{~\in~}lr}
        \tau & \unitInterval & \text{treasury expansion}\\
        \rho & \unitInterval & \text{monetary expansion}\\
        \var{d} & \unitInterval & \text{decentralization parameter}\\
        \var{k} & \Z & \text{optimal number of stake pools}\\
        \var{asc} & \unitInterval & \text{active slot coefficient}\\
      \end{array}
    \right)
  \end{equation*}
  %
  \emph{Pool Parameters}
  \begin{equation*}
    \PoolParameters =
    \left(
      \begin{array}{r@@{~\in~}lr}
        \var{cost} & \Coin & \text{pool's cost}\\
        \var{margin} & \Q & \text{pool's margin}\\
        \var{pledge} & \Coin & \text{pool's pledge}\\
        \var{owners} & \powerset{\StakeCredential} & \text{pool owners}\\
        \var{poolStakeCred} & \StakeCredential & \text{pool's stake credential}\\
      \end{array}
    \right)
  \end{equation*}
  %
  \emph{Pool Reward Information}
  \begin{equation*}
    \PoolRewardInfo =
    \left(
      \begin{array}{r@@{~\in~}lr}
        \var{pooledStake} & \StakeDistribution & \text{pool's stake distribution}\\
        \var{pooledStake} & \StakeDistribution & \text{pool's stake distribution}\\
        \var{poolRelativeStake} & \Q & \text{pool's relative stake}\\
        \var{ownerStake} & \Coin & \text{owner's combined stake}\\
        \var{poolPot} & \Coin & \text{pool's current reward pot}\\
        \var{poolPs} & \Coin & \text{pool parameters}\\
      \end{array}
    \right)
  \end{equation*}
  
  \caption{Record types}
  \label{fig:defs:record-types}
\end{figure*}

\begin{figure}[htb]
  \emph{Maximal Reward Function}
  %
  \begin{align*}
      & \fun{maxPool} \in \PParams \to \Coin \to \unitInterval \to \unitInterval \to \Coin \\
      & \fun{maxPool}~\var{pp}~\var{R}~\sigma~\var{p_r} =
          ~~~\Big\lfloor
             \frac{R}{1 + a_0}
             \cdot
             \left(
               \sigma' + p'\cdot a_0\cdot\frac{\sigma' - p'\frac{z_0-\sigma'}{z_0}}{z_0}
             \right)\Big\rfloor \\
      & ~~~\where \\
      & ~~~~~~~a_0 = \fun{influence}~pp \\
      & ~~~~~~~n_{opt} = \fun{nopt}~pp \\
      & ~~~~~~~z_0 = 1/n_{opt} \\
      & ~~~~~~~\sigma'=\min(\sigma,~z_0) \\
      & ~~~~~~~p'=\min(p_r,~z_0) \\
  \end{align*}

  \emph{Apparent Performance}
  %
  \begin{align*}
      & \fun{mkApparentPerformance} \in \unitInterval \to \unitInterval \to \N \to \N \to \Q \\
      & \fun{mkApparentPerformance}~{d}~{\sigma}~{n}~{\overline{N}} =
        \begin{cases}
          \frac{\beta}{\sigma} & \text{if } d < 0.8 \\
          1 & \text{otherwise}
        \end{cases} \\
      & ~~~\where \\
      & ~~~~~~~\beta = \frac{n}{\max(1, \overline{N})} \\
  \end{align*}
  \caption{Functions used in the Reward Calculation}
  \label{fig:functions:rewards}
\end{figure}

%\begin{figure}[htb]
%  \emph{Calculation to reward a single stake pool}
%  %
%  \begin{align*}
%    & \fun{rewardOnePool} \in \PParams \to \Coin \to \N \to \N \to \PoolParam\\
%      & ~~~\to \Stake \to \Q \to \Q \to \Coin \to \powerset{\AddrRWD}
%           \to (\AddrRWD \mapsto \Coin) \\
%      & \fun{rewardOnePool}~\var{pp}~\var{R}~\var{n}~\var{\overline{N}}~\var{pool}~\var{stake}~{\sigma}~{\sigma_a}~\var{tot}~\var{addrs_{rew}} =
%          \var{rewards}\\
%      & ~~~\where \\
%      & ~~~~~~~\var{ostake} = \sum_{\substack{
%        hk_\mapsto c\in\var{stake}\\
%        hk\in(\fun{poolOwners}~\var{pool})\\
%        }} c \\
%      & ~~~~~~~\var{pledge} = \fun{poolPledge}~pool \\
%      & ~~~~~~~p_{r} = \var{pledge} / \var{tot} \\
%      & ~~~~~~~maxP =
%      \begin{cases}
%        \fun{maxPool}~\var{pp}~\var{R}~\sigma~\var{p_r}&
%        \var{pledge} \leq \var{ostake}\\
%        0 & \text{otherwise.}
%      \end{cases} \\
%      & ~~~~~~~\var{appPerf} = \mkApparentPerformance{(\fun{d}~pp)}{\sigma_a}{n}{\overline{N}} \\
%      & ~~~~~~~\var{poolR} = \floor{\var{appPerf}\cdot\var{maxP}} \\
%      & ~~~~~~~\var{mRewards} = \\
%      & ~~~~~~~~~~\left\{
%                    \addrRw~hk\mapsto\mReward{poolR}{pool}{\frac{c}{tot}}{\sigma}
%                    ~\Big\vert~
%                    hk\mapsto c\in\var{stake},~~hk \not\in(\fun{poolOwners}~\var{pool})
%                  \right\}\\
%      & ~~~~~~~\var{lReward} = \lReward{poolR}{pool}{\frac{\var{ostake}}{tot}}{\sigma} \\
%      & ~~~~~~~\var{potentialRewards} =
%                 \var{mRewards} \cup
%                 \{(\fun{poolRAcnt}~\var{pool})\mapsto\var{lReward}\} \\
%      & ~~~~~~~\var{rewards} = \var{addrs_{rew}}\restrictdom{\var{potentialRewards}} \\
%  \end{align*}
%
%  \emph{Calculation to reward all stake pools}
%  %
%  \begin{align*}
%      & \fun{reward} \in \PParams \to \BlocksMade \to \Coin\to \powerset{\AddrRWD}
%      \to (\KeyHash \mapsto \PoolParam) \\
%      & ~~~\to \Stake \to (\KeyHash_{stake} \mapsto \KeyHash_{pool}) \to
%      \Coin \to (\AddrRWD \mapsto \Coin)\\
%      & \reward{pp}{blocks}{R}{addrs_{rew}}{poolParams}{stake}{delegs}{total}
%          = \var{rewards}\\
%      & ~~~\where \\
%      & ~~~~~~~\var{total}_a = \sum_{\_\mapsto c\in \var{stake}}c \\
%      & ~~~~~~~\var{\overline{N}} = \sum_{\_\mapsto m\in blocks}m \\
%      & ~~~~~~~pdata = \left\{
%        hk\mapsto \left(p,~n,~\poolStake{hk}{delegs}{stake}\right)
%        \mathrel{\Bigg|}
%        \begin{array}{r@{\mapsto}c@{~\in~}l}
%          hk & \var{p} & \var{poolParams} \\
%          hk & \var{n} & \var{blocks} \\
%        \end{array}
%      \right\} \\
%      & ~~~~~~~\var{results} = \\
%      & ~~~~~~~\left\{
%        hk \mapsto \fun{rewardOnePool}~
%                     \var{pp}~
%                     \var{R}~
%                     \var{n}~
%                     \var{\overline{N}}~
%                     \var{p}~
%                     \var{s}~
%                     \frac{\sum s}{total}~
%                     \frac{\sum s}{\var{total}_a}~
%                     \var{total}~
%                     \var{addrs_{rew}}
%                 \mid
%        hk\mapsto(p, n, s)\in\var{pdata} \right\} \\
%      & ~~~~~~~\var{rewards} = \bigcup_{\wcard\mapsto\var{r}\in\var{results}}\var{r}
%  \end{align*}
%  \caption{The Reward Calculation}
%  \label{fig:functions:reward-calc}
%\end{figure}


Coins and BlockCounts are integers.
>
> newtype Coin = Coin Integer
%if style == newcode
>   deriving (Show, Eq, Ord)
>   deriving (Semigroup, Monoid, Num) via Sum Integer
%endif
> newtype BlockCount = BlockCount { unBlockCount :: Integer }
%if style == newcode
>   deriving (Show, Eq, Ord)
>   deriving (Semigroup, Monoid, Num) via Sum Integer
%endif

Helper functions for Coins.
> 
> coinFloor :: Rational -> Coin
> coinFloor = Coin . floor
> 
> coinToQ :: Coin -> Rational
> coinToQ (Coin c) = fromIntegral c
> 
> coinRatio :: Coin -> Coin -> Rational
> coinRatio (Coin x) (Coin y) = x % y
> 

Abstract Stake Credentials and Pool IDs.
> newtype StakeCredential c = StakeCredential c
%if style == newcode
>   deriving (Show, Eq, Ord)
%endif
> 
> newtype PoolID p = PoolID p
%if style == newcode
>   deriving (Show, Eq, Ord)
%endif

Records for the Protocol Parameters, the Pool Parameters, and
PoolRewardInfo (which stores some intermediate calculations
used in for all the member rewards for a given pool).

> data PoolParameters c =
>   PoolParameters
>     { cost   :: Coin,
>       margin :: Rational,
>       pledge :: Coin,
>       owners :: Set (StakeCredential c),
>       poolStakeCred :: StakeCredential c
>     }
%if style == newcode
>     deriving (Show)
%endif
> 
> data ProtocolParameters =
>   ProtocolParameters
>     { asc  :: Rational
>     , d    :: Rational
>     , k :: Integer
>     , rho    :: Rational
>     , tau  :: Rational
>     }
%if style == newcode
>     deriving (Show)
%endif
> 
> data PoolRewardInfo c =
>   PoolRewardInfo
>     { pooledStake :: StakeDistribution c
>     , poolRelativeStake :: Rational
>     , ownerStake :: Coin
>     , poolPot :: Coin
>     , poolPs :: PoolParameters c
>     }
%if style == newcode
>     deriving (Show)
%endif

Finally, some type aliases for commonly used maps.
> type Rewards c           = Map (StakeCredential c) Coin
> type StakeDistribution c = Map (StakeCredential c) Coin
> type Delgations p c      = Map (StakeCredential c) (PoolID p)
> type Pools p c           = Map (PoolID p) (PoolParameters c)
> type BlocksMade p        = Map (PoolID p) BlockCount

\section{Calulating $R$, $totalStake$, and $blocksTotal$}

The reward calculation first must compute three values, $R$, $totalStake$, and $blocksTotal$.

> getRewardParameters ::
>   ProtocolParameters ->
>   BlocksMade p ->
>   Coin ->
>   Coin ->
>   Coin ->
>   Integer ->
>   (Coin, Coin, BlockCount)
> getRewardParameters pp blocks fees reserves maxSupply slotsPerEpoch =
>   (_R, totalStake, blocksTotal)
>   where
>     blocksTotal = sum blocks
>     expectedBlocks = floor $ (1 - d pp) * asc pp * fromIntegral slotsPerEpoch
>     eta
>       | d pp >= 0.8 = 1
>       | expectedBlocks == 0 = 0
>       | otherwise = unBlockCount blocksTotal % expectedBlocks
>     deltaR = coinFloor $ min 1 eta * rho pp * coinToQ reserves
>     rewardPot = fees + deltaR
>     deltaT = coinFloor $ tau pp * coinToQ rewardPot
>     _R = rewardPot - deltaT
>     totalStake = maxSupply - reserves

\section{Calulating per-pool values}
Each stake pool will need the values in $PoolRewardInfo$ to compute the member and leader rewards.
The most involved value, $poolPot$ (the total rewards available to this pool),
will require three helper functions: $maxPool$, $maxP$, $mkApparentPerformance$, and $poolR$.
The function $maxPool$ determines an upperbound of rewards that the stake pool could earn,
based on the main reward pot for this epoch, $R$, the pool's relative stake, the pool's cost and margin,
and the pool's pledge. The function $maxP$ checks that the stake pool has met its pledeg.
The function $mkApparentPerformance$ computes a performance adjustment, based on how many blocks it produced.
Finally, $poolR$ adjusts the pools potential reward pot by the performance adjustment.

> maxPool ::
>   ProtocolParameters ->
>   Coin ->
>   Rational ->
>   Rational ->
>   Coin
> maxPool pp r sigma pR = coinFloor $ factor1 * factor2
>   where
>     z0 = 1 % k pp
>     sigma' = min sigma z0
>     p' = min pR z0
>     factor1 :: Rational
>     factor1 = coinToQ r / (1 + asc pp)
>     factor2 = sigma' + p' * asc pp * factor3
>     factor3 = (sigma' - p' * factor4) / z0
>     factor4 = (z0 - sigma') / z0
> 
> mkApparentPerformance ::
>   ProtocolParameters ->
>   Rational ->
>   BlockCount ->
>   BlockCount ->
>   Rational
> mkApparentPerformance pp sigma (BlockCount blocksN) (BlockCount blocksTotal)
>   | sigma == 0 = 0
>   | d pp < 0.8 = beta / sigma
>   | otherwise = 1
>   where
>     beta = blocksN % max 1 blocksTotal
> 
> maxP ::
>   ProtocolParameters ->
>   PoolParameters c ->
>   Coin ->
>   Coin ->
>   Rational ->
>   Coin ->
>   Coin
> maxP pp pool r totalStake sigma ownerLovelace =
>   if pledge pool <= ownerLovelace
>     then maxPool pp r sigma pledgeRatio
>     else 0
>   where
>     pledgeRatio = coinRatio (pledge pool) totalStake
> 
> poolR ::
>   ProtocolParameters ->
>   PoolParameters c ->
>   Rational ->
>   BlockCount ->
>   BlockCount ->
>   Coin ->
>   Coin ->
>   Rational ->
>   Coin ->
>   Coin
> poolR pp pool sigmaA blocksN blocksTotal totalStake r sigma ownerLovelace =
>   coinFloor (appPerf * coinToQ mp)
>   where
>     appPerf = mkApparentPerformance pp sigmaA blocksN blocksTotal
>     mp = maxP pp pool r totalStake sigma ownerLovelace
>
> poolStake ::
>   (Eq p, Ord c) =>
>   PoolID p ->
>   Delgations p c ->
>   StakeDistribution c ->
>   StakeDistribution c
> poolStake poolID delegations =
>   Map.filterWithKey f
>   where
>     f cred _ =
>       case Map.lookup cred delegations of
>         Nothing -> False
>         Just p -> p == poolID
> 
> poolRewardInfo ::
>   (Ord p, Ord c) =>
>   ProtocolParameters ->
>   BlocksMade p ->
>   BlockCount ->
>   StakeDistribution c ->
>   Delgations p c ->
>   Coin ->
>   Coin ->
>   Coin ->
>   PoolID p ->
>   PoolParameters c ->
>   PoolRewardInfo c
> poolRewardInfo pp blocks blocksTotal sd delegations totalStake total_a r pid pool =
>   PoolRewardInfo { pooledStake = pstake
>                  , poolRelativeStake = sigma'
>                  , ownerStake = ostk
>                  , poolPot = poolR'
>                  , poolPs = pool
>                  }
>   where
>     pstake = poolStake pid delegations sd
>     pstakeTot = sum pstake
>     ostk = Set.foldl' (\c o -> c + Map.findWithDefault 0 o pstake) 0 (owners pool)
>     blocksN = Map.findWithDefault 0 pid blocks
>     sigma' = coinRatio pstakeTot totalStake
>     sigmaA' = coinRatio pstakeTot total_a
>     poolR' = poolR pp pool sigmaA' blocksN blocksTotal totalStake r sigma' ostk


\section{Individual Rewards}

Using the values computed in the previous two sections, we can now
compute member and leader rewards.

> leaderRew ::
>   Coin ->
>   PoolParameters c ->
>   Rational ->
>   Rational ->
>   Coin
> leaderRew f pool s sigma
>   | f <= c = f
>   | otherwise =
>     c <> coinFloor (coinToQ (f - c) * (m + (1 - m) * s / sigma))
>   where
>     c = cost pool
>     m = margin pool

> memberRew ::
>   Coin ->
>   PoolParameters c ->
>   Rational ->
>   Rational ->
>   Coin
> memberRew f pool t sigma
>   | f <= c = mempty
>   | otherwise =
>     coinFloor $ coinToQ (f - c) * (1 - m) * t / sigma
>   where
>     c = cost pool
>     m = margin pool

\section{Putting it all together: strategy A}

> rewardOnePool ::
>   Ord c =>
>   Coin ->
>   PoolParameters c ->
>   PoolRewardInfo c ->
>   Rewards c
> rewardOnePool
>   totalStake
>   pool
>   (PoolRewardInfo stake sigma ownerLovelace poolRPot poolRI) =
>     Map.insertWith (<>) (poolStakeCred pool) lReward mRewards
>     where
>       f c = memberRew poolRPot pool (coinRatio c totalStake) sigma
>       mRewards = Map.map f (Map.filterWithKey notPoolOwner stake)
>       notPoolOwner cred _ = cred `Set.notMember` owners poolRI
>       lReward = leaderRew poolRPot pool (coinRatio ownerLovelace totalStake) sigma

> rewardsA ::
>   (Ord p, Ord c) =>
>   ProtocolParameters ->
>   Pools p c ->
>   BlocksMade p ->
>   Delgations p c ->
>   StakeDistribution c ->
>   Coin ->
>   Coin ->
>   Coin ->
>   Integer ->
>   Rewards c
> rewardsA pp pools blocks delegations sd fees reserves maxSupply slotsPerEpoch =
>   Map.foldlWithKey f mempty pools
>   where
>     (_R, totalStake, blocksTotal) =
>       getRewardParameters pp blocks fees reserves maxSupply slotsPerEpoch
>     totA = sum sd
>     f acc pid pool =
>       Map.unionWith (<>) acc $
>         rewardOnePool
>           totalStake
>           pool
>           (poolRewardInfo pp blocks blocksTotal sd delegations totalStake totA _R pid pool)

\section{Putting it all together: strategy B}

> getPoolRewardParameters ::
>   (Ord p, Ord c) =>
>   ProtocolParameters ->
>   Pools p c ->
>   BlocksMade p ->
>   BlockCount ->
>   Delgations p c ->
>   StakeDistribution c ->
>   Coin ->
>   Coin ->
>   Map (PoolID p) (PoolRewardInfo c)
> getPoolRewardParameters pp pools blocks blocksTotal delegations sd totalStake r =
>   Map.mapWithKey f pools
>   where
>     f = poolRewardInfo pp blocks blocksTotal sd delegations totalStake totA r
>     totA = sum sd

> computeMemberRewards' ::
>   (Ord p, Ord c) =>
>   Coin ->
>   Map (PoolID p) (PoolRewardInfo c) ->
>   Delgations p c ->
>   StakeCredential c ->
>   Coin ->
>   Maybe Coin
> computeMemberRewards' totalStake poolRIs ds sc c = do
>   pid <- Map.lookup sc ds
>   poolRI <- Map.lookup pid poolRIs
>   if sc `Set.member` (owners . poolPs $ poolRI)
>     then Nothing
>     else Just $ memberRew
>            (poolPot poolRI)
>            (poolPs poolRI)
>            (coinRatio c totalStake)
>            (poolRelativeStake poolRI)
> 
> computeMemberRewards ::
>   (Ord p, Ord c) =>
>   Coin ->
>   Map (PoolID p) (PoolRewardInfo c) ->
>   Delgations p c ->
>   StakeCredential c ->
>   Coin ->
>   Coin
> computeMemberRewards totalStake poolRIs ds sc c =
>   fromMaybe 0 $ computeMemberRewards' totalStake poolRIs ds sc c
> 
> rewardsB ::
>   (Ord p, Ord c) =>
>   ProtocolParameters ->
>   Pools p c ->
>   BlocksMade p ->
>   Delgations p c ->
>   StakeDistribution c ->
>   Coin ->
>   Coin ->
>   Coin ->
>   Integer ->
>   Rewards c
> rewardsB pp pools blocks delegations sd fees reserves maxSupply slotsPerEpoch =
>   Map.unionWith (<>) mRs lRs
>   where
>     (r, totalStake, blocksTotal) =
>       getRewardParameters pp blocks fees reserves maxSupply slotsPerEpoch
>     poolInfos = getPoolRewardParameters pp pools blocks blocksTotal delegations sd totalStake r
>     mRs = Map.mapWithKey (computeMemberRewards totalStake poolInfos delegations) sd
>     f acc poolRI =
>       Map.insertWith
>         (<>)
>         (poolStakeCred . poolPs $ poolRI)
>         (leaderRew
>           (poolPot poolRI)
>           (poolPs poolRI)
>           (coinRatio (ownerStake poolRI) totalStake)
>           (poolRelativeStake poolRI)
>         )
>         acc
>     lRs = Map.foldl' f mempty poolInfos

\end{document}
