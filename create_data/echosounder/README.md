* 2024-03-13: I noticed that the numerical tests in create_echosounder.R fail.
  The plots look slightly different, also.  I don't know why this is.  But I
  also have no way of knowing if the tests are valid, because they are merely
  comparing against results I had noted in 2012 or a couple of years before.
  (Had these check values even corresponded to the setup used now?) I do not
  plan to look into this further because I don't have software that can check
  the values.  If I were looking at new echosounder data, then I might have
  access to official software that could be checked.  That is my criterion for
  spending time on the code, trying to regress across perhaps over a thousand
  commits to oce. I do not want to update the built-in dataset, either, because
  possibly it is in use by somebody in the community, so changing it ought to
  be for a well-supported reason.
