package object model {
  type Subscription = CommonSubscription with PaymentStatus
  type PaidSubscription = CommonSubscription with Paid
  type FreeSubscription = CommonSubscription with Free
}
