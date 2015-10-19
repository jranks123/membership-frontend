package model.Healthcheck

import com.gu.membership.zuora.rest.ProductCatalog
import model.TierPricing
import org.specs2.mutable.Specification

import scala.util.{Success, Failure}

class PricingCheckTest extends Specification {

  "PricingCheck" should {

    "Return an empty seq of messages when the option is false" in {
      PricingCheck(() => None).messages mustEqual Nil
    }

    "Return an empty seq when the Try is an exception" in {
      PricingCheck(() => Some(Failure(new Exception()))).messages mustEqual Nil
    }

    "Get some messages for invalid tier pricing" in {
      val tp = new TierPricing(new ProductCatalog(Nil))
      PricingCheck(() => Some(Success(tp))).messages must not have size (0)
    }
  }
}
