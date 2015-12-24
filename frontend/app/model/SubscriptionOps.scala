package model

import com.gu.membership.MembershipCatalog
import com.gu.memsub.Subscription
import com.gu.salesforce.{FreeTier, PaidTier, Tier}

object SubscriptionOps {
  implicit class WithTier(subscription: Subscription) {
    def tier(implicit catalog: MembershipCatalog): Tier =
      catalog.unsafePlan(subscription.productRatePlanId).tier

    def paidTier(implicit catalog: MembershipCatalog): PaidTier =
      catalog.unsafePaidPlan(subscription.productRatePlanId).tier

    def paidFreeTier(implicit catalog: MembershipCatalog): FreeTier =
      catalog.unsafeFindPaid(subscription.productRatePlanId).tier
  }

}
