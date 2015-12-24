package model

import com.gu.membership.MembershipCatalog
import com.gu.memsub.Subscription
import com.gu.salesforce.{FreeTier, PaidTier, Tier}

object SubscriptionOps {
  implicit class WithTier(subscription: Subscription) {
    def tier(implicit catalog: MembershipCatalog): Tier =
      catalog.unsafeFind(subscription.productRatePlanId).tier

    def paidTier(implicit catalog: MembershipCatalog): PaidTier =
      catalog.unsafeFindPaid(subscription.productRatePlanId).tier

    def freeTier(implicit catalog: MembershipCatalog): FreeTier =
      catalog.unsafeFindFree(subscription.productRatePlanId).tier
  }

}
