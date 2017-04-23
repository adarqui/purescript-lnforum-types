module LN.T.Internal.Lens where



import Data.Argonaut.Core               (jsonEmptyObject)
import Data.Argonaut.Decode             (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Combinators ((.?))
import Data.Argonaut.Encode             (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Combinators ((~>), (:=))
import Data.Argonaut.Printer            (printJson)
import Data.Date.Helpers                (Date)
import Data.Either                      (Either(..))
import Data.Foreign                     (ForeignError(..), fail)
import Data.Foreign.NullOrUndefined     (unNullOrUndefined)
import Data.Foreign.Class               (class IsForeign, read, readProp)
import Data.Maybe                       (Maybe(..))
import Data.Tuple                       (Tuple(..))
import Purescript.Api.Helpers           (class QueryParam, qp)
import Network.HTTP.Affjax.Request      (class Requestable, toRequest)
import Network.HTTP.Affjax.Response     (class Respondable, ResponseType(..))
import Optic.Core                       ((^.), (..))
import Optic.Types                      (Lens, Lens')
import Prelude                          (class Show, show, class Eq, eq, pure, bind, ($), (<>), (<$>), (<*>), (==), (&&))
import Data.Default

import Purescript.Api.Helpers

a_ :: forall b a r. Lens { a :: a | r } { a :: b | r } a b
a_ f o = o { a = _ } <$> f o.a


abbreviation_ :: forall b a r. Lens { abbreviation :: a | r } { abbreviation :: b | r } a b
abbreviation_ f o = o { abbreviation = _ } <$> f o.abbreviation


acceptTOS_ :: forall b a r. Lens { acceptTOS :: a | r } { acceptTOS :: b | r } a b
acceptTOS_ f o = o { acceptTOS = _ } <$> f o.acceptTOS


acceptedAt_ :: forall b a r. Lens { acceptedAt :: a | r } { acceptedAt :: b | r } a b
acceptedAt_ f o = o { acceptedAt = _ } <$> f o.acceptedAt


active_ :: forall b a r. Lens { active :: a | r } { active :: b | r } a b
active_ f o = o { active = _ } <$> f o.active


activityAt_ :: forall b a r. Lens { activityAt :: a | r } { activityAt :: b | r } a b
activityAt_ f o = o { activityAt = _ } <$> f o.activityAt


answer_ :: forall b a r. Lens { answer :: a | r } { answer :: b | r } a b
answer_ f o = o { answer = _ } <$> f o.answer


apiResponses_ :: forall b a r. Lens { apiResponses :: a | r } { apiResponses :: b | r } a b
apiResponses_ f o = o { apiResponses = _ } <$> f o.apiResponses


assocBy_ :: forall b a r. Lens { assocBy :: a | r } { assocBy :: b | r } a b
assocBy_ f o = o { assocBy = _ } <$> f o.assocBy


assocResult_ :: forall b a r. Lens { assocResult :: a | r } { assocResult :: b | r } a b
assocResult_ f o = o { assocResult = _ } <$> f o.assocResult


author_ :: forall b a r. Lens { author :: a | r } { author :: b | r } a b
author_ f o = o { author = _ } <$> f o.author


b_ :: forall b a r. Lens { b :: a | r } { b :: b | r } a b
b_ f o = o { b = _ } <$> f o.b


back_ :: forall b a r. Lens { back :: a | r } { back :: b | r } a b
back_ f o = o { back = _ } <$> f o.back


birthdate_ :: forall b a r. Lens { birthdate :: a | r } { birthdate :: b | r } a b
birthdate_ f o = o { birthdate = _ } <$> f o.birthdate


blockedAt_ :: forall b a r. Lens { blockedAt :: a | r } { blockedAt :: b | r } a b
blockedAt_ f o = o { blockedAt = _ } <$> f o.blockedAt


body_ :: forall b a r. Lens { body :: a | r } { body :: b | r } a b
body_ f o = o { body = _ } <$> f o.body


bucketResponses_ :: forall b a r. Lens { bucketResponses :: a | r } { bucketResponses :: b | r } a b
bucketResponses_ f o = o { bucketResponses = _ } <$> f o.bucketResponses


bucket_ :: forall b a r. Lens { bucket :: a | r } { bucket :: b | r } a b
bucket_ f o = o { bucket = _ } <$> f o.bucket


categories_ :: forall b a r. Lens { categories :: a | r } { categories :: b | r } a b
categories_ f o = o { categories = _ } <$> f o.categories


checksum_ :: forall b a r. Lens { checksum :: a | r } { checksum :: b | r } a b
checksum_ f o = o { checksum = _ } <$> f o.checksum


columns_ :: forall b a r. Lens { columns :: a | r } { columns :: b | r } a b
columns_ f o = o { columns = _ } <$> f o.columns


comment_ :: forall b a r. Lens { comment :: a | r } { comment :: b | r } a b
comment_ f o = o { comment = _ } <$> f o.comment


company_ :: forall b a r. Lens { company :: a | r } { company :: b | r } a b
company_ f o = o { company = _ } <$> f o.company


content_ :: forall b a r. Lens { content :: a | r } { content :: b | r } a b
content_ f o = o { content = _ } <$> f o.content


countResponses_ :: forall b a r. Lens { countResponses :: a | r } { countResponses :: b | r } a b
countResponses_ f o = o { countResponses = _ } <$> f o.countResponses


counter_ :: forall b a r. Lens { counter :: a | r } { counter :: b | r } a b
counter_ f o = o { counter = _ } <$> f o.counter


createdAt_ :: forall b a r. Lens { createdAt :: a | r } { createdAt :: b | r } a b
createdAt_ f o = o { createdAt = _ } <$> f o.createdAt


dataP_ :: forall b a r. Lens { dataP :: a | r } { dataP :: b | r } a b
dataP_ f o = o { dataP = _ } <$> f o.dataP


deactivatedAt_ :: forall b a r. Lens { deactivatedAt :: a | r } { deactivatedAt :: b | r } a b
deactivatedAt_ f o = o { deactivatedAt = _ } <$> f o.deactivatedAt


debug_ :: forall b a r. Lens { debug :: a | r } { debug :: b | r } a b
debug_ f o = o { debug = _ } <$> f o.debug


desc_ :: forall b a r. Lens { desc :: a | r } { desc :: b | r } a b
desc_ f o = o { desc = _ } <$> f o.desc


description_ :: forall b a r. Lens { description :: a | r } { description :: b | r } a b
description_ f o = o { description = _ } <$> f o.description


dislike_ :: forall b a r. Lens { dislike :: a | r } { dislike :: b | r } a b
dislike_ f o = o { dislike = _ } <$> f o.dislike


dislikes_ :: forall b a r. Lens { dislikes :: a | r } { dislikes :: b | r } a b
dislikes_ f o = o { dislikes = _ } <$> f o.dislikes


displayName_ :: forall b a r. Lens { displayName :: a | r } { displayName :: b | r } a b
displayName_ f o = o { displayName = _ } <$> f o.displayName


email_ :: forall b a r. Lens { email :: a | r } { email :: b | r } a b
email_ f o = o { email = _ } <$> f o.email


emailMD5_ :: forall b a r. Lens { emailMD5 :: a | r } { emailMD5 :: b | r } a b
emailMD5_ f o = o { emailMD5 = _ } <$> f o.emailMD5


emptyResponses_ :: forall b a r. Lens { emptyResponses :: a | r } { emptyResponses :: b | r } a b
emptyResponses_ f o = o { emptyResponses = _ } <$> f o.emptyResponses


ent_ :: forall b a r. Lens { ent :: a | r } { ent :: b | r } a b
ent_ f o = o { ent = _ } <$> f o.ent


entId_ :: forall b a r. Lens { entId :: a | r } { entId :: b | r } a b
entId_ f o = o { entId = _ } <$> f o.entId


examples_ :: forall b a r. Lens { examples :: a | r } { examples :: b | r } a b
examples_ f o = o { examples = _ } <$> f o.examples


fact_ :: forall b a r. Lens { fact :: a | r } { fact :: b | r } a b
fact_ f o = o { fact = _ } <$> f o.fact


filters_ :: forall b a r. Lens { filters :: a | r } { filters :: b | r } a b
filters_ f o = o { filters = _ } <$> f o.filters


front_ :: forall b a r. Lens { front :: a | r } { front :: b | r } a b
front_ f o = o { front = _ } <$> f o.front


fullName_ :: forall b a r. Lens { fullName :: a | r } { fullName :: b | r } a b
fullName_ f o = o { fullName = _ } <$> f o.fullName


gender_ :: forall b a r. Lens { gender :: a | r } { gender :: b | r } a b
gender_ f o = o { gender = _ } <$> f o.gender


guard_ :: forall b a r. Lens { guard :: a | r } { guard :: b | r } a b
guard_ f o = o { guard = _ } <$> f o.guard


icon_ :: forall b a r. Lens { icon :: a | r } { icon :: b | r } a b
icon_ f o = o { icon = _ } <$> f o.icon


id_ :: forall b a r. Lens { id :: a | r } { id :: b | r } a b
id_ f o = o { id = _ } <$> f o.id


ident_ :: forall b a r. Lens { ident :: a | r } { ident :: b | r } a b
ident_ f o = o { ident = _ } <$> f o.ident


idTarget_ :: forall b a r. Lens { idTarget :: a | r } { idTarget :: b | r } a b
idTarget_ f o = o { idTarget = _ } <$> f o.idTarget


imageUrl_ :: forall b a r. Lens { imageUrl :: a | r } { imageUrl :: b | r } a b
imageUrl_ f o = o { imageUrl = _ } <$> f o.imageUrl


isAccepted_ :: forall b a r. Lens { isAccepted :: a | r } { isAccepted :: b | r } a b
isAccepted_ f o = o { isAccepted = _ } <$> f o.isAccepted


isAnonymous_ :: forall b a r. Lens { isAnonymous :: a | r } { isAnonymous :: b | r } a b
isAnonymous_ f o = o { isAnonymous = _ } <$> f o.isAnonymous


isBlocked_ :: forall b a r. Lens { isBlocked :: a | r } { isBlocked :: b | r } a b
isBlocked_ f o = o { isBlocked = _ } <$> f o.isBlocked


isNew_ :: forall b a r. Lens { isNew :: a | r } { isNew :: b | r } a b
isNew_ f o = o { isNew = _ } <$> f o.isNew


isOwner_ :: forall b a r. Lens { isOwner :: a | r } { isOwner :: b | r } a b
isOwner_ f o = o { isOwner = _ } <$> f o.isOwner


isRead_ :: forall b a r. Lens { isRead :: a | r } { isRead :: b | r } a b
isRead_ f o = o { isRead = _ } <$> f o.isRead


isSaved_ :: forall b a r. Lens { isSaved :: a | r } { isSaved :: b | r } a b
isSaved_ f o = o { isSaved = _ } <$> f o.isSaved


isStarred_ :: forall b a r. Lens { isStarred :: a | r } { isStarred :: b | r } a b
isStarred_ f o = o { isStarred = _ } <$> f o.isStarred


karmaBad_ :: forall b a r. Lens { karmaBad :: a | r } { karmaBad :: b | r } a b
karmaBad_ f o = o { karmaBad = _ } <$> f o.karmaBad


karmaGood_ :: forall b a r. Lens { karmaGood :: a | r } { karmaGood :: b | r } a b
karmaGood_ f o = o { karmaGood = _ } <$> f o.karmaGood


key_ :: forall b a r. Lens { key :: a | r } { key :: b | r } a b
key_ f o = o { key = _ } <$> f o.key


label_ :: forall b a r. Lens { label :: a | r } { label :: b | r } a b
label_ f o = o { label = _ } <$> f o.label


leuron_ :: forall b a r. Lens { leuron :: a | r } { leuron :: b | r } a b
leuron_ f o = o { leuron = _ } <$> f o.leuron


leuronId_ :: forall b a r. Lens { leuronId :: a | r } { leuronId :: b | r } a b
leuronId_ f o = o { leuronId = _ } <$> f o.leuronId


leuronPackResponses_ :: forall b a r. Lens { leuronPackResponses :: a | r } { leuronPackResponses :: b | r } a b
leuronPackResponses_ f o = o { leuronPackResponses = _ } <$> f o.leuronPackResponses


leuronResponses_ :: forall b a r. Lens { leuronResponses :: a | r } { leuronResponses :: b | r } a b
leuronResponses_ f o = o { leuronResponses = _ } <$> f o.leuronResponses


leuronStatResponses_ :: forall b a r. Lens { leuronStatResponses :: a | r } { leuronStatResponses :: b | r } a b
leuronStatResponses_ f o = o { leuronStatResponses = _ } <$> f o.leuronStatResponses


leuronTrainingId_ :: forall b a r. Lens { leuronTrainingId :: a | r } { leuronTrainingId :: b | r } a b
leuronTrainingId_ f o = o { leuronTrainingId = _ } <$> f o.leuronTrainingId


leuronTrainingResponses_ :: forall b a r. Lens { leuronTrainingResponses :: a | r } { leuronTrainingResponses :: b | r } a b
leuronTrainingResponses_ f o = o { leuronTrainingResponses = _ } <$> f o.leuronTrainingResponses


leuronTrainingStatResponses_ :: forall b a r. Lens { leuronTrainingStatResponses :: a | r } { leuronTrainingStatResponses :: b | r } a b
leuronTrainingStatResponses_ f o = o { leuronTrainingStatResponses = _ } <$> f o.leuronTrainingStatResponses


leurons_ :: forall b a r. Lens { leurons :: a | r } { leurons :: b | r } a b
leurons_ f o = o { leurons = _ } <$> f o.leurons


like_ :: forall b a r. Lens { like :: a | r } { like :: b | r } a b
like_ f o = o { like = _ } <$> f o.like


likeResponses_ :: forall b a r. Lens { likeResponses :: a | r } { likeResponses :: b | r } a b
likeResponses_ f o = o { likeResponses = _ } <$> f o.likeResponses


likeStatResponses_ :: forall b a r. Lens { likeStatResponses :: a | r } { likeStatResponses :: b | r } a b
likeStatResponses_ f o = o { likeStatResponses = _ } <$> f o.likeStatResponses


likes_ :: forall b a r. Lens { likes :: a | r } { likes :: b | r } a b
likes_ f o = o { likes = _ } <$> f o.likes


list_ :: forall b a r. Lens { list :: a | r } { list :: b | r } a b
list_ f o = o { list = _ } <$> f o.list


location_ :: forall b a r. Lens { location :: a | r } { location :: b | r } a b
location_ f o = o { location = _ } <$> f o.location


locked_ :: forall b a r. Lens { locked :: a | r } { locked :: b | r } a b
locked_ f o = o { locked = _ } <$> f o.locked


meaning_ :: forall b a r. Lens { meaning :: a | r } { meaning :: b | r } a b
meaning_ f o = o { meaning = _ } <$> f o.meaning


members_ :: forall b a r. Lens { members :: a | r } { members :: b | r } a b
members_ f o = o { members = _ } <$> f o.members


membership_ :: forall b a r. Lens { membership :: a | r } { membership :: b | r } a b
membership_ f o = o { membership = _ } <$> f o.membership


modifiedAt_ :: forall b a r. Lens { modifiedAt :: a | r } { modifiedAt :: b | r } a b
modifiedAt_ f o = o { modifiedAt = _ } <$> f o.modifiedAt


modifiedBy_ :: forall b a r. Lens { modifiedBy :: a | r } { modifiedBy :: b | r } a b
modifiedBy_ f o = o { modifiedBy = _ } <$> f o.modifiedBy


motwLimit_ :: forall b a r. Lens { motwLimit :: a | r } { motwLimit :: b | r } a b
motwLimit_ f o = o { motwLimit = _ } <$> f o.motwLimit


msg_ :: forall b a r. Lens { msg :: a | r } { msg :: b | r } a b
msg_ f o = o { msg = _ } <$> f o.msg


n_ :: forall b a r. Lens { n :: a | r } { n :: b | r } a b
n_ f o = o { n = _ } <$> f o.n


name_ :: forall b a r. Lens { name :: a | r } { name :: b | r } a b
name_ f o = o { name = _ } <$> f o.name


neutral_ :: forall b a r. Lens { neutral :: a | r } { neutral :: b | r } a b
neutral_ f o = o { neutral = _ } <$> f o.neutral


opt_ :: forall b a r. Lens { opt :: a | r } { opt :: b | r } a b
opt_ f o = o { opt = _ } <$> f o.opt


page_ :: forall b a r. Lens { page :: a | r } { page :: b | r } a b
page_ f o = o { page = _ } <$> f o.page


parentFolderId_ :: forall b a r. Lens { parentFolderId :: a | r } { parentFolderId :: b | r } a b
parentFolderId_ f o = o { parentFolderId = _ } <$> f o.parentFolderId


parentId_ :: forall b a r. Lens { parentId :: a | r } { parentId :: b | r } a b
parentId_ f o = o { parentId = _ } <$> f o.parentId


permissions_ :: forall b a r. Lens { permissions :: a | r } { permissions :: b | r } a b
permissions_ f o = o { permissions = _ } <$> f o.permissions


plugin_ :: forall b a r. Lens { plugin :: a | r } { plugin :: b | r } a b
plugin_ f o = o { plugin = _ } <$> f o.plugin


poll_ :: forall b a r. Lens { poll :: a | r } { poll :: b | r } a b
poll_ f o = o { poll = _ } <$> f o.poll


prerequisites_ :: forall b a r. Lens { prerequisites :: a | r } { prerequisites :: b | r } a b
prerequisites_ f o = o { prerequisites = _ } <$> f o.prerequisites


privateTags_ :: forall b a r. Lens { privateTags :: a | r } { privateTags :: b | r } a b
privateTags_ f o = o { privateTags = _ } <$> f o.privateTags


profile_ :: forall b a r. Lens { profile :: a | r } { profile :: b | r } a b
profile_ f o = o { profile = _ } <$> f o.profile


profileEmail_ :: forall b a r. Lens { profileEmail :: a | r } { profileEmail :: b | r } a b
profileEmail_ f o = o { profileEmail = _ } <$> f o.profileEmail


profileId_ :: forall b a r. Lens { profileId :: a | r } { profileId :: b | r } a b
profileId_ f o = o { profileId = _ } <$> f o.profileId


profileLogin_ :: forall b a r. Lens { profileLogin :: a | r } { profileLogin :: b | r } a b
profileLogin_ f o = o { profileLogin = _ } <$> f o.profileLogin


profileName_ :: forall b a r. Lens { profileName :: a | r } { profileName :: b | r } a b
profileName_ f o = o { profileName = _ } <$> f o.profileName


profileResponses_ :: forall b a r. Lens { profileResponses :: a | r } { profileResponses :: b | r } a b
profileResponses_ f o = o { profileResponses = _ } <$> f o.profileResponses


question_ :: forall b a r. Lens { question :: a | r } { question :: b | r } a b
question_ f o = o { question = _ } <$> f o.question


reason_ :: forall b a r. Lens { reason :: a | r } { reason :: b | r } a b
reason_ f o = o { reason = _ } <$> f o.reason


resource_ :: forall b a r. Lens { resource :: a | r } { resource :: b | r } a b
resource_ f o = o { resource = _ } <$> f o.resource


resourceId_ :: forall b a r. Lens { resourceId :: a | r } { resourceId :: b | r } a b
resourceId_ f o = o { resourceId = _ } <$> f o.resourceId


resourcePackResponses_ :: forall b a r. Lens { resourcePackResponses :: a | r } { resourcePackResponses :: b | r } a b
resourcePackResponses_ f o = o { resourcePackResponses = _ } <$> f o.resourcePackResponses


resourceResponses_ :: forall b a r. Lens { resourceResponses :: a | r } { resourceResponses :: b | r } a b
resourceResponses_ f o = o { resourceResponses = _ } <$> f o.resourceResponses


resourceStatResponses_ :: forall b a r. Lens { resourceStatResponses :: a | r } { resourceStatResponses :: b | r } a b
resourceStatResponses_ f o = o { resourceStatResponses = _ } <$> f o.resourceStatResponses


resources_ :: forall b a r. Lens { resources :: a | r } { resources :: b | r } a b
resources_ f o = o { resources = _ } <$> f o.resources


respect_ :: forall b a r. Lens { respect :: a | r } { respect :: b | r } a b
respect_ f o = o { respect = _ } <$> f o.respect


rows_ :: forall b a r. Lens { rows :: a | r } { rows :: b | r } a b
rows_ f o = o { rows = _ } <$> f o.rows


score_ :: forall b a r. Lens { score :: a | r } { score :: b | r } a b
score_ f o = o { score = _ } <$> f o.score


scoreHi_ :: forall b a r. Lens { scoreHi :: a | r } { scoreHi :: b | r } a b
scoreHi_ f o = o { scoreHi = _ } <$> f o.scoreHi


scoreLo_ :: forall b a r. Lens { scoreLo :: a | r } { scoreLo :: b | r } a b
scoreLo_ f o = o { scoreLo = _ } <$> f o.scoreLo


section_ :: forall b a r. Lens { section :: a | r } { section :: b | r } a b
section_ f o = o { section = _ } <$> f o.section


signature_ :: forall b a r. Lens { signature :: a | r } { signature :: b | r } a b
signature_ f o = o { signature = _ } <$> f o.signature


simpleInt_ :: forall b a r. Lens { simpleInt :: a | r } { simpleInt :: b | r } a b
simpleInt_ f o = o { simpleInt = _ } <$> f o.simpleInt


simpleInts_ :: forall b a r. Lens { simpleInts :: a | r } { simpleInts :: b | r } a b
simpleInts_ f o = o { simpleInts = _ } <$> f o.simpleInts


simpleString_ :: forall b a r. Lens { simpleString :: a | r } { simpleString :: b | r } a b
simpleString_ f o = o { simpleString = _ } <$> f o.simpleString


simpleStrings_ :: forall b a r. Lens { simpleStrings :: a | r } { simpleStrings :: b | r } a b
simpleStrings_ f o = o { simpleStrings = _ } <$> f o.simpleStrings


source_ :: forall b a r. Lens { source :: a | r } { source :: b | r } a b
source_ f o = o { source = _ } <$> f o.source


splits_ :: forall b a r. Lens { splits :: a | r } { splits :: b | r } a b
splits_ f o = o { splits = _ } <$> f o.splits


star_ :: forall b a r. Lens { star :: a | r } { star :: b | r } a b
star_ f o = o { star = _ } <$> f o.star


starResponses_ :: forall b a r. Lens { starResponses :: a | r } { starResponses :: b | r } a b
starResponses_ f o = o { starResponses = _ } <$> f o.starResponses


starStatResponses_ :: forall b a r. Lens { starStatResponses :: a | r } { starStatResponses :: b | r } a b
starStatResponses_ f o = o { starStatResponses = _ } <$> f o.starStatResponses


stars_ :: forall b a r. Lens { stars :: a | r } { stars :: b | r } a b
stars_ f o = o { stars = _ } <$> f o.stars


stat_ :: forall b a r. Lens { stat :: a | r } { stat :: b | r } a b
stat_ f o = o { stat = _ } <$> f o.stat


sticky_ :: forall b a r. Lens { sticky :: a | r } { sticky :: b | r } a b
sticky_ f o = o { sticky = _ } <$> f o.sticky


strengths_ :: forall b a r. Lens { strengths :: a | r } { strengths :: b | r } a b
strengths_ f o = o { strengths = _ } <$> f o.strengths


style_ :: forall b a r. Lens { style :: a | r } { style :: b | r } a b
style_ f o = o { style = _ } <$> f o.style


subject_ :: forall b a r. Lens { subject :: a | r } { subject :: b | r } a b
subject_ f o = o { subject = _ } <$> f o.subject


substitutions_ :: forall b a r. Lens { substitutions :: a | r } { substitutions :: b | r } a b
substitutions_ f o = o { substitutions = _ } <$> f o.substitutions


suggestedTags_ :: forall b a r. Lens { suggestedTags :: a | r } { suggestedTags :: b | r } a b
suggestedTags_ f o = o { suggestedTags = _ } <$> f o.suggestedTags


summary_ :: forall b a r. Lens { summary :: a | r } { summary :: b | r } a b
summary_ f o = o { summary = _ } <$> f o.summary


system_ :: forall b a r. Lens { system :: a | r } { system :: b | r } a b
system_ f o = o { system = _ } <$> f o.system


tags_ :: forall b a r. Lens { tags :: a | r } { tags :: b | r } a b
tags_ f o = o { tags = _ } <$> f o.tags


template_ :: forall b a r. Lens { template :: a | r } { template :: b | r } a b
template_ f o = o { template = _ } <$> f o.template


testResponses_ :: forall b a r. Lens { testResponses :: a | r } { testResponses :: b | r } a b
testResponses_ f o = o { testResponses = _ } <$> f o.testResponses


text_ :: forall b a r. Lens { text :: a | r } { text :: b | r } a b
text_ f o = o { text = _ } <$> f o.text


title_ :: forall b a r. Lens { title :: a | r } { title :: b | r } a b
title_ f o = o { title = _ } <$> f o.title


toUserId_ :: forall b a r. Lens { toUserId :: a | r } { toUserId :: b | r } a b
toUserId_ f o = o { toUserId = _ } <$> f o.toUserId


training_ :: forall b a r. Lens { training :: a | r } { training :: b | r } a b
training_ f o = o { training = _ } <$> f o.training


url_ :: forall b a r. Lens { url :: a | r } { url :: b | r } a b
url_ f o = o { url = _ } <$> f o.url


urls_ :: forall b a r. Lens { urls :: a | r } { urls :: b | r } a b
urls_ f o = o { urls = _ } <$> f o.urls


user_ :: forall b a r. Lens { user :: a | r } { user :: b | r } a b
user_ f o = o { user = _ } <$> f o.user


userId_ :: forall b a r. Lens { userId :: a | r } { userId :: b | r } a b
userId_ f o = o { userId = _ } <$> f o.userId


userPackResponses_ :: forall b a r. Lens { userPackResponses :: a | r } { userPackResponses :: b | r } a b
userPackResponses_ f o = o { userPackResponses = _ } <$> f o.userPackResponses


userResponses_ :: forall b a r. Lens { userResponses :: a | r } { userResponses :: b | r } a b
userResponses_ f o = o { userResponses = _ } <$> f o.userResponses


userSanitizedPackResponses_ :: forall b a r. Lens { userSanitizedPackResponses :: a | r } { userSanitizedPackResponses :: b | r } a b
userSanitizedPackResponses_ f o = o { userSanitizedPackResponses = _ } <$> f o.userSanitizedPackResponses


userSanitizedResponses_ :: forall b a r. Lens { userSanitizedResponses :: a | r } { userSanitizedResponses :: b | r } a b
userSanitizedResponses_ f o = o { userSanitizedResponses = _ } <$> f o.userSanitizedResponses


userSanitizedStatResponses_ :: forall b a r. Lens { userSanitizedStatResponses :: a | r } { userSanitizedStatResponses :: b | r } a b
userSanitizedStatResponses_ f o = o { userSanitizedStatResponses = _ } <$> f o.userSanitizedStatResponses


value_ :: forall b a r. Lens { value :: a | r } { value :: b | r } a b
value_ f o = o { value = _ } <$> f o.value


values_ :: forall b a r. Lens { values :: a | r } { values :: b | r } a b
values_ f o = o { values = _ } <$> f o.values


version_ :: forall b a r. Lens { version :: a | r } { version :: b | r } a b
version_ f o = o { version = _ } <$> f o.version


views_ :: forall b a r. Lens { views :: a | r } { views :: b | r } a b
views_ f o = o { views = _ } <$> f o.views


visibility_ :: forall b a r. Lens { visibility :: a | r } { visibility :: b | r } a b
visibility_ f o = o { visibility = _ } <$> f o.visibility


website_ :: forall b a r. Lens { website :: a | r } { website :: b | r } a b
website_ f o = o { website = _ } <$> f o.website

-- footer









--
-- manual
--


bucketNode_ :: forall b a r. Lens { bucketNode :: a | r } { bucketNode :: b | r } a b
bucketNode_ f o = o { bucketNode = _ } <$> f o.bucketNode


bucketRound_ :: forall b a r. Lens { bucketRound :: a | r } { bucketRound :: b | r } a b
bucketRound_ f o = o { bucketRound = _ } <$> f o.bucketRound


trainingStyles_ :: forall b a r. Lens { trainingStyles :: a | r } { trainingStyles :: b | r } a b
trainingStyles_ f o = o { trainingStyles = _ } <$> f o.trainingStyles


threshold_ :: forall b a r. Lens { threshold :: a | r } { threshold :: b | r } a b
threshold_ f o = o { threshold = _ } <$> f o.threshold


timeLimit_ :: forall b a r. Lens { timeLimit :: a | r } { timeLimit :: b | r } a b
timeLimit_ f o = o { timeLimit = _ } <$> f o.timeLimit


timeLimitExceeded_ :: forall b a r. Lens { timeLimitExceeded :: a | r } { timeLimitExceeded :: b | r } a b
timeLimitExceeded_ f o = o { timeLimitExceeded = _ } <$> f o.timeLimitExceeded


trainingNode_ :: forall b a r. Lens { trainingNode :: a | r } { trainingNode :: b | r } a b
trainingNode_ f o = o { trainingNode = _ } <$> f o.trainingNode


numTotal_ :: forall b a r. Lens { numTotal :: a | r } { numTotal :: b | r } a b
numTotal_ f o = o { numTotal = _ } <$> f o.numTotal


numKnow_ :: forall b a r. Lens { numKnow :: a | r } { numKnow :: b | r } a b
numKnow_ f o = o { numKnow = _ } <$> f o.numKnow


numDontKnow_ :: forall b a r. Lens { numDontKnow :: a | r } { numDontKnow :: b | r } a b
numDontKnow_ f o = o { numDontKnow = _ } <$> f o.numDontKnow


numDontCare_ :: forall b a r. Lens { numDontCare :: a | r } { numDontCare :: b | r } a b
numDontCare_ f o = o { numDontCare = _ } <$> f o.numDontCare


numProtest_ :: forall b a r. Lens { numProtest :: a | r } { numProtest :: b | r } a b
numProtest_ f o = o { numProtest = _ } <$> f o.numProtest


honorKnow_ :: forall b a r. Lens { honorKnow :: a | r } { honorKnow :: b | r } a b
honorKnow_ f o = o { honorKnow = _ } <$> f o.honorKnow

honorDontKnow_ :: forall b a r. Lens { honorDontKnow :: a | r } { honorDontKnow :: b | r } a b
honorDontKnow_ f o = o { honorDontKnow = _ } <$> f o.honorDontKnow

honorDontCare_ :: forall b a r. Lens { honorDontCare :: a | r } { honorDontCare :: b | r } a b
honorDontCare_ f o = o { honorDontCare = _ } <$> f o.honorDontCare

honorProtest_ :: forall b a r. Lens { honorProtest :: a | r } { honorProtest :: b | r } a b
honorProtest_ f o = o { honorProtest = _ } <$> f o.honorProtest

honorKnowAt_ :: forall b a r. Lens { honorKnowAt :: a | r } { honorKnowAt :: b | r } a b
honorKnowAt_ f o = o { honorKnowAt = _ } <$> f o.honorKnowAt

honorDontKnowAt_ :: forall b a r. Lens { honorDontKnowAt :: a | r } { honorDontKnowAt :: b | r } a b
honorDontKnowAt_ f o = o { honorDontKnowAt = _ } <$> f o.honorDontKnowAt

honorDontCareAt_ :: forall b a r. Lens { honorDontCareAt :: a | r } { honorDontCareAt :: b | r } a b
honorDontCareAt_ f o = o { honorDontCareAt = _ } <$> f o.honorDontCareAt

honorProtestAt_ :: forall b a r. Lens { honorProtestAt :: a | r } { honorProtestAt :: b | r } a b
honorProtestAt_ f o = o { honorProtestAt = _ } <$> f o.honorProtestAt



booleanKnow_ :: forall b a r. Lens { booleanKnow :: a | r } { booleanKnow :: b | r } a b
booleanKnow_ f o = o { booleanKnow = _ } <$> f o.booleanKnow

booleanDontKnow_ :: forall b a r. Lens { booleanDontKnow :: a | r } { booleanDontKnow :: b | r } a b
booleanDontKnow_ f o = o { booleanDontKnow = _ } <$> f o.booleanDontKnow

booleanDontCare_ :: forall b a r. Lens { booleanDontCare :: a | r } { booleanDontCare :: b | r } a b
booleanDontCare_ f o = o { booleanDontCare = _ } <$> f o.booleanDontCare

booleanProtest_ :: forall b a r. Lens { booleanProtest :: a | r } { booleanProtest :: b | r } a b
booleanProtest_ f o = o { booleanProtest = _ } <$> f o.booleanProtest

booleanKnowAt_ :: forall b a r. Lens { booleanKnowAt :: a | r } { booleanKnowAt :: b | r } a b
booleanKnowAt_ f o = o { booleanKnowAt = _ } <$> f o.booleanKnowAt

booleanDontKnowAt_ :: forall b a r. Lens { booleanDontKnowAt :: a | r } { booleanDontKnowAt :: b | r } a b
booleanDontKnowAt_ f o = o { booleanDontKnowAt = _ } <$> f o.booleanDontKnowAt

booleanDontCareAt_ :: forall b a r. Lens { booleanDontCareAt :: a | r } { booleanDontCareAt :: b | r } a b
booleanDontCareAt_ f o = o { booleanDontCareAt = _ } <$> f o.booleanDontCareAt

booleanProtestAt_ :: forall b a r. Lens { booleanProtestAt :: a | r } { booleanProtestAt :: b | r } a b
booleanProtestAt_ f o = o { booleanProtestAt = _ } <$> f o.booleanProtestAt



matchKnow_ :: forall b a r. Lens { matchKnow :: a | r } { matchKnow :: b | r } a b
matchKnow_ f o = o { matchKnow = _ } <$> f o.matchKnow

matchDontKnow_ :: forall b a r. Lens { matchDontKnow :: a | r } { matchDontKnow :: b | r } a b
matchDontKnow_ f o = o { matchDontKnow = _ } <$> f o.matchDontKnow

matchDontCare_ :: forall b a r. Lens { matchDontCare :: a | r } { matchDontCare :: b | r } a b
matchDontCare_ f o = o { matchDontCare = _ } <$> f o.matchDontCare

matchProtest_ :: forall b a r. Lens { matchProtest :: a | r } { matchProtest :: b | r } a b
matchProtest_ f o = o { matchProtest = _ } <$> f o.matchProtest

matchKnowAt_ :: forall b a r. Lens { matchKnowAt :: a | r } { matchKnowAt :: b | r } a b
matchKnowAt_ f o = o { matchKnowAt = _ } <$> f o.matchKnowAt

matchDontKnowAt_ :: forall b a r. Lens { matchDontKnowAt :: a | r } { matchDontKnowAt :: b | r } a b
matchDontKnowAt_ f o = o { matchDontKnowAt = _ } <$> f o.matchDontKnowAt

matchDontCareAt_ :: forall b a r. Lens { matchDontCareAt :: a | r } { matchDontCareAt :: b | r } a b
matchDontCareAt_ f o = o { matchDontCareAt = _ } <$> f o.matchDontCareAt

matchProtestAt_ :: forall b a r. Lens { matchProtestAt :: a | r } { matchProtestAt :: b | r } a b
matchProtestAt_ f o = o { matchProtestAt = _ } <$> f o.matchProtestAt



subsKnow_ :: forall b a r. Lens { subsKnow :: a | r } { subsKnow :: b | r } a b
subsKnow_ f o = o { subsKnow = _ } <$> f o.subsKnow

subsDontKnow_ :: forall b a r. Lens { subsDontKnow :: a | r } { subsDontKnow :: b | r } a b
subsDontKnow_ f o = o { subsDontKnow = _ } <$> f o.subsDontKnow

subsDontCare_ :: forall b a r. Lens { subsDontCare :: a | r } { subsDontCare :: b | r } a b
subsDontCare_ f o = o { subsDontCare = _ } <$> f o.subsDontCare

subsProtest_ :: forall b a r. Lens { subsProtest :: a | r } { subsProtest :: b | r } a b
subsProtest_ f o = o { subsProtest = _ } <$> f o.subsProtest

subsKnowAt_ :: forall b a r. Lens { subsKnowAt :: a | r } { subsKnowAt :: b | r } a b
subsKnowAt_ f o = o { subsKnowAt = _ } <$> f o.subsKnowAt

subsDontKnowAt_ :: forall b a r. Lens { subsDontKnowAt :: a | r } { subsDontKnowAt :: b | r } a b
subsDontKnowAt_ f o = o { subsDontKnowAt = _ } <$> f o.subsDontKnowAt

subsDontCareAt_ :: forall b a r. Lens { subsDontCareAt :: a | r } { subsDontCareAt :: b | r } a b
subsDontCareAt_ f o = o { subsDontCareAt = _ } <$> f o.subsDontCareAt

subsProtestAt_ :: forall b a r. Lens { subsProtestAt :: a | r } { subsProtestAt :: b | r } a b
subsProtestAt_ f o = o { subsProtestAt = _ } <$> f o.subsProtestAt



splitsKnow_ :: forall b a r. Lens { splitsKnow :: a | r } { splitsKnow :: b | r } a b
splitsKnow_ f o = o { splitsKnow = _ } <$> f o.splitsKnow

splitsDontKnow_ :: forall b a r. Lens { splitsDontKnow :: a | r } { splitsDontKnow :: b | r } a b
splitsDontKnow_ f o = o { splitsDontKnow = _ } <$> f o.splitsDontKnow

splitsDontCare_ :: forall b a r. Lens { splitsDontCare :: a | r } { splitsDontCare :: b | r } a b
splitsDontCare_ f o = o { splitsDontCare = _ } <$> f o.splitsDontCare

splitsProtest_ :: forall b a r. Lens { splitsProtest :: a | r } { splitsProtest :: b | r } a b
splitsProtest_ f o = o { splitsProtest = _ } <$> f o.splitsProtest

splitsKnowAt_ :: forall b a r. Lens { splitsKnowAt :: a | r } { splitsKnowAt :: b | r } a b
splitsKnowAt_ f o = o { splitsKnowAt = _ } <$> f o.splitsKnowAt

splitsDontKnowAt_ :: forall b a r. Lens { splitsDontKnowAt :: a | r } { splitsDontKnowAt :: b | r } a b
splitsDontKnowAt_ f o = o { splitsDontKnowAt = _ } <$> f o.splitsDontKnowAt

splitsDontCareAt_ :: forall b a r. Lens { splitsDontCareAt :: a | r } { splitsDontCareAt :: b | r } a b
splitsDontCareAt_ f o = o { splitsDontCareAt = _ } <$> f o.splitsDontCareAt

splitsProtestAt_ :: forall b a r. Lens { splitsProtestAt :: a | r } { splitsProtestAt :: b | r } a b
splitsProtestAt_ f o = o { splitsProtestAt = _ } <$> f o.splitsProtestAt

