package forms

import com.gu.i18n._
import com.gu.memsub.BillingPeriod._
import com.gu.memsub.Subscription.ProductRatePlanId
import com.gu.memsub.{Address, BillingPeriod, FullName}
import com.gu.salesforce.PaidTier
import model.FeatureChoice
import play.api.data.Forms._
import play.api.data.format.Formatter
import play.api.data.{Form, FormError, Mapping}

object MemberForm {
  case class NameForm(first: String, last: String) extends FullName

  case class PaymentForm(billingPeriod: BillingPeriod, token: String)

  case class MarketingChoicesForm(gnm: Option[Boolean], thirdParty: Option[Boolean])

  trait JoinForm {
    val name: NameForm
    val deliveryAddress: Address
    val marketingChoices: MarketingChoicesForm
    val password: Option[String]
    val planId: ProductRatePlanId
  }

  case class FriendJoinForm(name: NameForm, deliveryAddress: Address, marketingChoices: MarketingChoicesForm,
                            password: Option[String], planId: ProductRatePlanId) extends JoinForm

  case class StaffJoinForm(name: NameForm, deliveryAddress: Address, marketingChoices: MarketingChoicesForm,
                            password: Option[String], planId: ProductRatePlanId) extends JoinForm

  case class PaidMemberJoinForm(tier: PaidTier,
                                name: NameForm,
                                payment: PaymentForm,
                                deliveryAddress: Address,
                                billingAddress: Option[Address],
                                marketingChoices: MarketingChoicesForm,
                                password: Option[String],
                                casId: Option[String],
                                subscriberOffer: Boolean,
                                featureChoice: Set[FeatureChoice],
                                planId: ProductRatePlanId
                               ) extends JoinForm {
    lazy val zuoraAccountAddress = billingAddress.getOrElse(deliveryAddress)
  }

  case class AddressDetails(deliveryAddress: Address, billingAddress: Option[Address])

  trait MemberChangeForm {
    val featureChoice: Set[FeatureChoice]
    val addressDetails: Option[AddressDetails]
    def planId: ProductRatePlanId
  }

  case class PaidMemberChangeForm(password: String, featureChoice: Set[FeatureChoice], planId: ProductRatePlanId) extends MemberChangeForm {
    val addressDetails = None
  }

  case class FreeMemberChangeForm(payment: PaymentForm,
                                  deliveryAddress: Address,
                                  billingAddress: Option[Address],
                                  featureChoice: Set[FeatureChoice],
                                  planId: ProductRatePlanId) extends MemberChangeForm {
    val addressDetails = Some(AddressDetails(deliveryAddress, billingAddress))
  }

  case class FeedbackForm(category: String, page: String, feedback: String, name: String, email: String)

  implicit val productFeaturesFormatter: Formatter[Set[FeatureChoice]] = new Formatter[Set[FeatureChoice]] {
    override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], Set[FeatureChoice]] = {
      val inputVal = data.getOrElse(key, "")
      Right(FeatureChoice.setFromString(inputVal))
    }

    override def unbind(key: String, choices: Set[FeatureChoice]): Map[String, String] =
      Map(key -> FeatureChoice.setToString(choices))
  }

  implicit val countryFormatter: Formatter[Country] = new Formatter[Country] {
    override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], Country] = {
      val countryCode = data.get(key)
      lazy val formError = FormError(key, s"Cannot find a country by country code ${countryCode.getOrElse("")}")

      countryCode
        .flatMap(CountryGroup.countryByCode)
        .toRight[Seq[FormError]](Seq(formError))
    }

    override def unbind(key: String, value: Country): Map[String, String] =
      Map(key -> value.alpha2)
  }

  private val productFeature = of[Set[FeatureChoice]] as productFeaturesFormatter
  private val country = of[Country] as countryFormatter

  val planId = nonEmptyText.transform[ProductRatePlanId](ProductRatePlanId, _.get)

  val nonPaidAddressMapping: Mapping[Address] = mapping(
    "lineOne" -> text,
    "lineTwo" -> text,
    "town" -> text,
    "countyOrState" -> text,
    "postCode" -> text(maxLength=20),
    "country" -> country
  )(Address.apply)(Address.unapply).verifying(_.valid)

  val paidAddressMapping: Mapping[Address] = mapping(
    "lineOne" -> nonEmptyText,
    "lineTwo" -> text,
    "town" -> nonEmptyText,
    "countyOrState" -> text,
    "postCode" -> text(maxLength=20),
    "country" -> country
  )(Address.apply)(Address.unapply).verifying(_.valid)

  val nameMapping: Mapping[NameForm] = mapping(
    "first" -> nonEmptyText,
    "last" -> nonEmptyText
  )(NameForm.apply)(NameForm.unapply)

  val marketingChoicesMapping: Mapping[MarketingChoicesForm] = mapping(
    "gnnMarketing" -> optional(boolean),
    "thirdParty" -> optional(boolean)
  )(MarketingChoicesForm.apply)(MarketingChoicesForm.unapply)

  val paymentMapping: Mapping[PaymentForm] = mapping(
    "type" -> nonEmptyText.transform[BillingPeriod](b =>
      if (Seq("annual","subscriberOfferAnnual").contains(b)) year else month, _.noun),
    "token" -> nonEmptyText
  )(PaymentForm.apply)(PaymentForm.unapply)

  val feedbackMapping: Mapping[FeedbackForm] =   mapping(
    "category" -> nonEmptyText,
    "page" -> text,
    "feedback" -> nonEmptyText,
    "name" -> nonEmptyText,
    "email" -> email
  )(FeedbackForm.apply)(FeedbackForm.unapply)

  val friendJoinForm: Form[FriendJoinForm] = Form(
    mapping(
      "name" -> nameMapping,
      "deliveryAddress" -> nonPaidAddressMapping,
      "marketingChoices" -> marketingChoicesMapping,
      "password" -> optional(nonEmptyText),
      "planId" -> planId
    )(FriendJoinForm.apply)(FriendJoinForm.unapply)
  )

  val staffJoinForm: Form[StaffJoinForm] = Form(
    mapping(
      "name" -> nameMapping,
      "deliveryAddress" -> nonPaidAddressMapping,
      "marketingChoices" -> marketingChoicesMapping,
      "password" -> optional(nonEmptyText),
      "planId" -> planId
    )(StaffJoinForm.apply)(StaffJoinForm.unapply)
  )

  val paidMemberJoinForm: Form[PaidMemberJoinForm] = Form(
    mapping(
      "tier" -> nonEmptyText.transform[PaidTier](PaidTier.slugMap, _.slug),
      "name" -> nameMapping,
      "payment" -> paymentMapping,
      "deliveryAddress" -> paidAddressMapping,
      "billingAddress" -> optional(paidAddressMapping),
      "marketingChoices" -> marketingChoicesMapping,
      "password" -> optional(nonEmptyText),
      "casId" -> optional(nonEmptyText),
      "subscriberOffer" -> default(boolean, false),
      "featureChoice" -> productFeature,
      "planId" -> planId
    )(PaidMemberJoinForm.apply)(PaidMemberJoinForm.unapply)
  )

  val freeMemberChangeForm: Form[FreeMemberChangeForm] = Form(
    mapping(
      "payment" -> paymentMapping,
      "deliveryAddress" -> paidAddressMapping,
      "billingAddress" -> optional(paidAddressMapping),
      "featureChoice" -> productFeature,
      "planId" -> planId
    )(FreeMemberChangeForm.apply)(FreeMemberChangeForm.unapply)
  )

  val paidMemberChangeForm: Form[PaidMemberChangeForm] = Form(
    mapping(
      "password" -> nonEmptyText,
      "featureChoice" -> productFeature,
      "planId" -> planId
    )(PaidMemberChangeForm.apply)(PaidMemberChangeForm.unapply)
  )

  val feedbackForm: Form[FeedbackForm] = Form(
    mapping(
      "category" -> nonEmptyText,
      "page" -> text,
      "feedback" -> nonEmptyText,
      "name" -> nonEmptyText,
      "email" -> email
    )(FeedbackForm.apply)(FeedbackForm.unapply)
  )
}
