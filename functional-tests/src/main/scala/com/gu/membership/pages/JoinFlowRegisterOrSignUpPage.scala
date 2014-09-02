package com.gu.membership.pages

import org.openqa.selenium.{By, WebDriver}

/**
 * Created by jao on 14/08/2014.
 */
class JoinFlowRegisterOrSignUpPage(driver: WebDriver) extends BaseMembershipPage(driver) {

  private def signInButton = driver.findElement(By.xpath("//div[1]/section/div[3]/div[1]/a"))

  private def registerButton = driver.findElement(By.xpath("//div[1]/section/div[3]/div[2]/a"))

  def clickSignIn = {
    signInButton.click()
    new LoginPage(driver)
  }

  def clickRegister = {
    registerButton.click()
    new RegisterPage(driver)
  }
}