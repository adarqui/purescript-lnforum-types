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


board_ :: forall b a r. Lens { board :: a | r } { board :: b | r } a b
board_ f o = o { board = _ } <$> f o.board


boardId_ :: forall b a r. Lens { boardId :: a | r } { boardId :: b | r } a b
boardId_ f o = o { boardId = _ } <$> f o.boardId


boardPackResponses_ :: forall b a r. Lens { boardPackResponses :: a | r } { boardPackResponses :: b | r } a b
boardPackResponses_ f o = o { boardPackResponses = _ } <$> f o.boardPackResponses


boardResponses_ :: forall b a r. Lens { boardResponses :: a | r } { boardResponses :: b | r } a b
boardResponses_ f o = o { boardResponses = _ } <$> f o.boardResponses


boardStatResponses_ :: forall b a r. Lens { boardStatResponses :: a | r } { boardStatResponses :: b | r } a b
boardStatResponses_ f o = o { boardStatResponses = _ } <$> f o.boardStatResponses


boards_ :: forall b a r. Lens { boards :: a | r } { boards :: b | r } a b
boards_ f o = o { boards = _ } <$> f o.boards


body_ :: forall b a r. Lens { body :: a | r } { body :: b | r } a b
body_ f o = o { body = _ } <$> f o.body


bucketResponses_ :: forall b a r. Lens { bucketResponses :: a | r } { bucketResponses :: b | r } a b
bucketResponses_ f o = o { bucketResponses = _ } <$> f o.bucketResponses


canCreateSubBoards_ :: forall b a r. Lens { canCreateSubBoards :: a | r } { canCreateSubBoards :: b | r } a b
canCreateSubBoards_ f o = o { canCreateSubBoards = _ } <$> f o.canCreateSubBoards


canCreateThreads_ :: forall b a r. Lens { canCreateThreads :: a | r } { canCreateThreads :: b | r } a b
canCreateThreads_ f o = o { canCreateThreads = _ } <$> f o.canCreateThreads


categories_ :: forall b a r. Lens { categories :: a | r } { categories :: b | r } a b
categories_ f o = o { categories = _ } <$> f o.categories


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


forum_ :: forall b a r. Lens { forum :: a | r } { forum :: b | r } a b
forum_ f o = o { forum = _ } <$> f o.forum


forumId_ :: forall b a r. Lens { forumId :: a | r } { forumId :: b | r } a b
forumId_ f o = o { forumId = _ } <$> f o.forumId


forumPackResponses_ :: forall b a r. Lens { forumPackResponses :: a | r } { forumPackResponses :: b | r } a b
forumPackResponses_ f o = o { forumPackResponses = _ } <$> f o.forumPackResponses


forumResponses_ :: forall b a r. Lens { forumResponses :: a | r } { forumResponses :: b | r } a b
forumResponses_ f o = o { forumResponses = _ } <$> f o.forumResponses


forumStatResponses_ :: forall b a r. Lens { forumStatResponses :: a | r } { forumStatResponses :: b | r } a b
forumStatResponses_ f o = o { forumStatResponses = _ } <$> f o.forumStatResponses


forums_ :: forall b a r. Lens { forums :: a | r } { forums :: b | r } a b
forums_ f o = o { forums = _ } <$> f o.forums


front_ :: forall b a r. Lens { front :: a | r } { front :: b | r } a b
front_ f o = o { front = _ } <$> f o.front


fullName_ :: forall b a r. Lens { fullName :: a | r } { fullName :: b | r } a b
fullName_ f o = o { fullName = _ } <$> f o.fullName


gender_ :: forall b a r. Lens { gender :: a | r } { gender :: b | r } a b
gender_ f o = o { gender = _ } <$> f o.gender


globalGroup_ :: forall b a r. Lens { globalGroup :: a | r } { globalGroup :: b | r } a b
globalGroup_ f o = o { globalGroup = _ } <$> f o.globalGroup


globalGroupId_ :: forall b a r. Lens { globalGroupId :: a | r } { globalGroupId :: b | r } a b
globalGroupId_ f o = o { globalGroupId = _ } <$> f o.globalGroupId


globalGroupPackResponses_ :: forall b a r. Lens { globalGroupPackResponses :: a | r } { globalGroupPackResponses :: b | r } a b
globalGroupPackResponses_ f o = o { globalGroupPackResponses = _ } <$> f o.globalGroupPackResponses


globalGroupResponses_ :: forall b a r. Lens { globalGroupResponses :: a | r } { globalGroupResponses :: b | r } a b
globalGroupResponses_ f o = o { globalGroupResponses = _ } <$> f o.globalGroupResponses


globalGroupStatResponses_ :: forall b a r. Lens { globalGroupStatResponses :: a | r } { globalGroupStatResponses :: b | r } a b
globalGroupStatResponses_ f o = o { globalGroupStatResponses = _ } <$> f o.globalGroupStatResponses


group_ :: forall b a r. Lens { group :: a | r } { group :: b | r } a b
group_ f o = o { group = _ } <$> f o.group


groupId_ :: forall b a r. Lens { groupId :: a | r } { groupId :: b | r } a b
groupId_ f o = o { groupId = _ } <$> f o.groupId


groupMember_ :: forall b a r. Lens { groupMember :: a | r } { groupMember :: b | r } a b
groupMember_ f o = o { groupMember = _ } <$> f o.groupMember


groupMemberId_ :: forall b a r. Lens { groupMemberId :: a | r } { groupMemberId :: b | r } a b
groupMemberId_ f o = o { groupMemberId = _ } <$> f o.groupMemberId


groupMemberPackResponses_ :: forall b a r. Lens { groupMemberPackResponses :: a | r } { groupMemberPackResponses :: b | r } a b
groupMemberPackResponses_ f o = o { groupMemberPackResponses = _ } <$> f o.groupMemberPackResponses


groupMemberResponses_ :: forall b a r. Lens { groupMemberResponses :: a | r } { groupMemberResponses :: b | r } a b
groupMemberResponses_ f o = o { groupMemberResponses = _ } <$> f o.groupMemberResponses


groupPackResponses_ :: forall b a r. Lens { groupPackResponses :: a | r } { groupPackResponses :: b | r } a b
groupPackResponses_ f o = o { groupPackResponses = _ } <$> f o.groupPackResponses


groupResponses_ :: forall b a r. Lens { groupResponses :: a | r } { groupResponses :: b | r } a b
groupResponses_ f o = o { groupResponses = _ } <$> f o.groupResponses


groupStatResponses_ :: forall b a r. Lens { groupStatResponses :: a | r } { groupStatResponses :: b | r } a b
groupStatResponses_ f o = o { groupStatResponses = _ } <$> f o.groupStatResponses


groups_ :: forall b a r. Lens { groups :: a | r } { groups :: b | r } a b
groups_ f o = o { groups = _ } <$> f o.groups


guard_ :: forall b a r. Lens { guard :: a | r } { guard :: b | r } a b
guard_ f o = o { guard = _ } <$> f o.guard


icon_ :: forall b a r. Lens { icon :: a | r } { icon :: b | r } a b
icon_ f o = o { icon = _ } <$> f o.icon


id_ :: forall b a r. Lens { id :: a | r } { id :: b | r } a b
id_ f o = o { id = _ } <$> f o.id


ident_ :: forall b a r. Lens { ident :: a | r } { ident :: b | r } a b
ident_ f o = o { ident = _ } <$> f o.ident


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


latestThread_ :: forall b a r. Lens { latestThread :: a | r } { latestThread :: b | r } a b
latestThread_ f o = o { latestThread = _ } <$> f o.latestThread


latestThreadPost_ :: forall b a r. Lens { latestThreadPost :: a | r } { latestThreadPost :: b | r } a b
latestThreadPost_ f o = o { latestThreadPost = _ } <$> f o.latestThreadPost


latestThreadPostUser_ :: forall b a r. Lens { latestThreadPostUser :: a | r } { latestThreadPostUser :: b | r } a b
latestThreadPostUser_ f o = o { latestThreadPostUser = _ } <$> f o.latestThreadPostUser


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


orgId_ :: forall b a r. Lens { orgId :: a | r } { orgId :: b | r } a b
orgId_ f o = o { orgId = _ } <$> f o.orgId


organization_ :: forall b a r. Lens { organization :: a | r } { organization :: b | r } a b
organization_ f o = o { organization = _ } <$> f o.organization


organizationId_ :: forall b a r. Lens { organizationId :: a | r } { organizationId :: b | r } a b
organizationId_ f o = o { organizationId = _ } <$> f o.organizationId


organizationPackResponses_ :: forall b a r. Lens { organizationPackResponses :: a | r } { organizationPackResponses :: b | r } a b
organizationPackResponses_ f o = o { organizationPackResponses = _ } <$> f o.organizationPackResponses


organizationResponses_ :: forall b a r. Lens { organizationResponses :: a | r } { organizationResponses :: b | r } a b
organizationResponses_ f o = o { organizationResponses = _ } <$> f o.organizationResponses


organizationStatResponses_ :: forall b a r. Lens { organizationStatResponses :: a | r } { organizationStatResponses :: b | r } a b
organizationStatResponses_ f o = o { organizationStatResponses = _ } <$> f o.organizationStatResponses


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


pmId_ :: forall b a r. Lens { pmId :: a | r } { pmId :: b | r } a b
pmId_ f o = o { pmId = _ } <$> f o.pmId


pmIn_ :: forall b a r. Lens { pmIn :: a | r } { pmIn :: b | r } a b
pmIn_ f o = o { pmIn = _ } <$> f o.pmIn


pmInId_ :: forall b a r. Lens { pmInId :: a | r } { pmInId :: b | r } a b
pmInId_ f o = o { pmInId = _ } <$> f o.pmInId


pmInPackResponses_ :: forall b a r. Lens { pmInPackResponses :: a | r } { pmInPackResponses :: b | r } a b
pmInPackResponses_ f o = o { pmInPackResponses = _ } <$> f o.pmInPackResponses


pmInResponses_ :: forall b a r. Lens { pmInResponses :: a | r } { pmInResponses :: b | r } a b
pmInResponses_ f o = o { pmInResponses = _ } <$> f o.pmInResponses


pmOut_ :: forall b a r. Lens { pmOut :: a | r } { pmOut :: b | r } a b
pmOut_ f o = o { pmOut = _ } <$> f o.pmOut


pmOutId_ :: forall b a r. Lens { pmOutId :: a | r } { pmOutId :: b | r } a b
pmOutId_ f o = o { pmOutId = _ } <$> f o.pmOutId


pmOutPackResponses_ :: forall b a r. Lens { pmOutPackResponses :: a | r } { pmOutPackResponses :: b | r } a b
pmOutPackResponses_ f o = o { pmOutPackResponses = _ } <$> f o.pmOutPackResponses


pmOutResponses_ :: forall b a r. Lens { pmOutResponses :: a | r } { pmOutResponses :: b | r } a b
pmOutResponses_ f o = o { pmOutResponses = _ } <$> f o.pmOutResponses


pmResponses_ :: forall b a r. Lens { pmResponses :: a | r } { pmResponses :: b | r } a b
pmResponses_ f o = o { pmResponses = _ } <$> f o.pmResponses


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


recentPostsLimit_ :: forall b a r. Lens { recentPostsLimit :: a | r } { recentPostsLimit :: b | r } a b
recentPostsLimit_ f o = o { recentPostsLimit = _ } <$> f o.recentPostsLimit


recentThreadsLimit_ :: forall b a r. Lens { recentThreadsLimit :: a | r } { recentThreadsLimit :: b | r } a b
recentThreadsLimit_ f o = o { recentThreadsLimit = _ } <$> f o.recentThreadsLimit


reminderFolderResponses_ :: forall b a r. Lens { reminderFolderResponses :: a | r } { reminderFolderResponses :: b | r } a b
reminderFolderResponses_ f o = o { reminderFolderResponses = _ } <$> f o.reminderFolderResponses


reminderResponses_ :: forall b a r. Lens { reminderResponses :: a | r } { reminderResponses :: b | r } a b
reminderResponses_ f o = o { reminderResponses = _ } <$> f o.reminderResponses


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


team_ :: forall b a r. Lens { team :: a | r } { team :: b | r } a b
team_ f o = o { team = _ } <$> f o.team


teamId_ :: forall b a r. Lens { teamId :: a | r } { teamId :: b | r } a b
teamId_ f o = o { teamId = _ } <$> f o.teamId


teamMember_ :: forall b a r. Lens { teamMember :: a | r } { teamMember :: b | r } a b
teamMember_ f o = o { teamMember = _ } <$> f o.teamMember


teamMemberId_ :: forall b a r. Lens { teamMemberId :: a | r } { teamMemberId :: b | r } a b
teamMemberId_ f o = o { teamMemberId = _ } <$> f o.teamMemberId


teamMemberPackResponses_ :: forall b a r. Lens { teamMemberPackResponses :: a | r } { teamMemberPackResponses :: b | r } a b
teamMemberPackResponses_ f o = o { teamMemberPackResponses = _ } <$> f o.teamMemberPackResponses


teamMemberResponses_ :: forall b a r. Lens { teamMemberResponses :: a | r } { teamMemberResponses :: b | r } a b
teamMemberResponses_ f o = o { teamMemberResponses = _ } <$> f o.teamMemberResponses


teamMemberStatResponses_ :: forall b a r. Lens { teamMemberStatResponses :: a | r } { teamMemberStatResponses :: b | r } a b
teamMemberStatResponses_ f o = o { teamMemberStatResponses = _ } <$> f o.teamMemberStatResponses


teamPackResponses_ :: forall b a r. Lens { teamPackResponses :: a | r } { teamPackResponses :: b | r } a b
teamPackResponses_ f o = o { teamPackResponses = _ } <$> f o.teamPackResponses


teamResponses_ :: forall b a r. Lens { teamResponses :: a | r } { teamResponses :: b | r } a b
teamResponses_ f o = o { teamResponses = _ } <$> f o.teamResponses


teamStatResponses_ :: forall b a r. Lens { teamStatResponses :: a | r } { teamStatResponses :: b | r } a b
teamStatResponses_ f o = o { teamStatResponses = _ } <$> f o.teamStatResponses


teams_ :: forall b a r. Lens { teams :: a | r } { teams :: b | r } a b
teams_ f o = o { teams = _ } <$> f o.teams


template_ :: forall b a r. Lens { template :: a | r } { template :: b | r } a b
template_ f o = o { template = _ } <$> f o.template


testResponses_ :: forall b a r. Lens { testResponses :: a | r } { testResponses :: b | r } a b
testResponses_ f o = o { testResponses = _ } <$> f o.testResponses


text_ :: forall b a r. Lens { text :: a | r } { text :: b | r } a b
text_ f o = o { text = _ } <$> f o.text


thread_ :: forall b a r. Lens { thread :: a | r } { thread :: b | r } a b
thread_ f o = o { thread = _ } <$> f o.thread


threadId_ :: forall b a r. Lens { threadId :: a | r } { threadId :: b | r } a b
threadId_ f o = o { threadId = _ } <$> f o.threadId


threadPackResponses_ :: forall b a r. Lens { threadPackResponses :: a | r } { threadPackResponses :: b | r } a b
threadPackResponses_ f o = o { threadPackResponses = _ } <$> f o.threadPackResponses


threadPost_ :: forall b a r. Lens { threadPost :: a | r } { threadPost :: b | r } a b
threadPost_ f o = o { threadPost = _ } <$> f o.threadPost


threadPostId_ :: forall b a r. Lens { threadPostId :: a | r } { threadPostId :: b | r } a b
threadPostId_ f o = o { threadPostId = _ } <$> f o.threadPostId


threadPostPackResponses_ :: forall b a r. Lens { threadPostPackResponses :: a | r } { threadPostPackResponses :: b | r } a b
threadPostPackResponses_ f o = o { threadPostPackResponses = _ } <$> f o.threadPostPackResponses


threadPostResponses_ :: forall b a r. Lens { threadPostResponses :: a | r } { threadPostResponses :: b | r } a b
threadPostResponses_ f o = o { threadPostResponses = _ } <$> f o.threadPostResponses


threadPostStatResponses_ :: forall b a r. Lens { threadPostStatResponses :: a | r } { threadPostStatResponses :: b | r } a b
threadPostStatResponses_ f o = o { threadPostStatResponses = _ } <$> f o.threadPostStatResponses


threadPosts_ :: forall b a r. Lens { threadPosts :: a | r } { threadPosts :: b | r } a b
threadPosts_ f o = o { threadPosts = _ } <$> f o.threadPosts


threadPostsPerThread_ :: forall b a r. Lens { threadPostsPerThread :: a | r } { threadPostsPerThread :: b | r } a b
threadPostsPerThread_ f o = o { threadPostsPerThread = _ } <$> f o.threadPostsPerThread


threadResponses_ :: forall b a r. Lens { threadResponses :: a | r } { threadResponses :: b | r } a b
threadResponses_ f o = o { threadResponses = _ } <$> f o.threadResponses


threadStatResponses_ :: forall b a r. Lens { threadStatResponses :: a | r } { threadStatResponses :: b | r } a b
threadStatResponses_ f o = o { threadStatResponses = _ } <$> f o.threadStatResponses


threads_ :: forall b a r. Lens { threads :: a | r } { threads :: b | r } a b
threads_ f o = o { threads = _ } <$> f o.threads


threadsPerBoard_ :: forall b a r. Lens { threadsPerBoard :: a | r } { threadsPerBoard :: b | r } a b
threadsPerBoard_ f o = o { threadsPerBoard = _ } <$> f o.threadsPerBoard


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


withBoard_ :: forall b a r. Lens { withBoard :: a | r } { withBoard :: b | r } a b
withBoard_ f o = o { withBoard = _ } <$> f o.withBoard


withForum_ :: forall b a r. Lens { withForum :: a | r } { withForum :: b | r } a b
withForum_ f o = o { withForum = _ } <$> f o.withForum


withOrganization_ :: forall b a r. Lens { withOrganization :: a | r } { withOrganization :: b | r } a b
withOrganization_ f o = o { withOrganization = _ } <$> f o.withOrganization


withThread_ :: forall b a r. Lens { withThread :: a | r } { withThread :: b | r } a b
withThread_ f o = o { withThread = _ } <$> f o.withThread


workouts_ :: forall b a r. Lens { workouts :: a | r } { workouts :: b | r } a b
workouts_ f o = o { workouts = _ } <$> f o.workouts

-- footer
