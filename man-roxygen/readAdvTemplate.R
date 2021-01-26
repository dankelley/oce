#' Read an ADV data file
#'
#' Read an ADV data file, producing an object of type `adv`. This
#' function works by transferring control to a more specialized function,
#' e.g. [read.adp.nortek()] and [read.adp.sontek()],
#' and in many cases users will find it preferable to either use these or
#' the several even more specialized functions, if the file type is
#' known.
#'
#' @details
#'
#' Files *without* headers may be created in experiments in which a data
#' logger was set up to monitor the serial data stream from an instrument.  The
#' lack of header information places a burden on the user, who must supply such
#' basic information as the times of observations, the instrument orientation,
#' the instrument coordinate system, etc.  Example 3 below shows how to deal
#' with such files.  Three things should be noted.
#'
#' 1. The user must choose the appropriate `read.adv` variant
#' corresponding to the instrument in question.  (This is necessary because
#' [oceMagic()], which is used by the generic [read.oce()]
#' routine, cannot determine the type of instrument by examining a file that
#' lacks a header.)
#'
#' 2. The call to the `read` function must include a start time
#' (`start`) and the number of seconds between data (`deltat`),
#' again, because the instrument data stream may lack those things when the
#' device is set to a serial mode.  Also, of course, it is necessary to set
#' `header=FALSE` in the function call.
#'
#' 3. Once the file has been read in, the user will be obliged to specify
#' other information, for the object to be well-formed.  For example, the
#' `read` function will have no way of knowing the instrument orientation,
#' the coordinate system being used, the transformation matrix to go from
#' `"beam"` to `"xyz"` coordinates, or the instrument heading, pitch,
#' and roll, to go from `"xyz"` coordinates to `"enu"` coordinates.
#' Such things are illustrated in example 3 below.
#'
#' In ADV data files, velocities are coded to signed 2-byte integers, with a
#' scale factor being used to convert to velocity in metres per second.  These
#' two facts control the maximum recordable velocity and the velocity
#' resolution, values that may be retrieved for an ADV object name `d`
#' with `d[["velocityMaximum"]]` and `d[["velocityResolution"]]`.
#'
#' @param file a connection or a character string giving the name of the file
#' to load.  It is also possible to give `file` as a vector of filenames,
#' to handle the case of data split into files by a data logger.  In the
#' multi-file case, `header` must be `FALSE`, `start` must be a
#' vector of times, and `deltat` must be provided.
#'
#' @param from index number of the first profile to be read, or the time of
#' that profile, as created with [as.POSIXct()] (hint: use
#' `tz="UTC"`).  This argument is ignored if `header==FALSE`. See
#' \dQuote{Examples}.
#'
#' @param to indication of the last profile to read, in a format matching that
#' of `from`.  This is ignored if `header==FALSE`.
#'
#' @param by an indication of the stride length to use while walking through
#' the file.  This is ignored if `header==FALSE`.  Otherwise, if this is
#' an integer, then `by-1` profiles are skipped between each pair of
#' profiles that is read. This may not make much sense, if the data are not
#' equi-spaced in time.  If `by` is a string representing a time interval,
#' in colon-separated format, then this interval is divided by the sampling
#' interval, to get the stride length. *BUG:* `by` only partially
#' works; see \dQuote{Bugs}.
#'
#' @param longitude optional signed number indicating the longitude in degrees
#' East.
#'
#' @param latitude optional signed number indicating the latitude in degrees
#' North.
#'
#' @param tz character string indicating time zone to be assumed in the data.
#'
#' @param debug a flag that turns on debugging.  The value indicates the depth
#' within the call stack to which debugging applies.  For example,
#' `read.adv.nortek()` calls `read.header.nortek()`, so that
#' `read.adv.nortek(...,debug=2)` provides information about not just the
#' main body of the data file, but also the details of the header.
#'
#' @param monitor boolean value indicating whether to indicate the progress
#' of reading the file, by using [txtProgressBar()] or otherwise.  The value
#' of `monitor` is changed to `FALSE` automatically, for non-interactive
#' sessions.
#'
#' @param processingLog if provided, the action item to be stored in the log.
#' This parameter is typically only provided for internal calls; the default
#' that it provides is better for normal calls by a user.
#'
#' @return An [adv-class] object that contains measurements made with an ADV device.
#'
#' The `metadata` contains information as given in the following table.
#' The ``Nortek name'' is the name used in the Nortek System Integrator Guide
#' (reference 1) and the ``Sontek name'' is the name used in the relevant
#' Sontek documentation.  References are given in square brackets.
#'
#' \tabular{llll}{
#'
#' **`metadata` name**\tab **Nortek name** \tab **Sontek name** \tab **Meaning**\cr
#'
#' `manufacturer`\tab - \tab - \tab Either `"nortek"` or `"sontek"`\cr
#'
#' `instrumentType`\tab  - \tab - \tab Either `"vector"` or `"adv"`\cr
#'
#' `filename`\tab  - \tab - \tab Name of data file(s)\cr
#'
#' `latitude`\tab - \tab - \tab  Latitude of mooring (if applicable)\cr
#'
#' `longitude`\tab - \tab - \tab Longitude of mooring (if applicable)\cr
#'
#' `numberOfSamples`\tab - \tab - \tab Number of data samples in file\cr
#'
#' `numberOfBeams`\tab NBeams (reference 1, p18) \tab - \tab Number of beams (always 3)\cr
#'
#' `numberOfBeamSequencesPerBurst`\tab NPings \tab - \tab number of beam sequences per burst\cr
#'
#' `measurementInterval`\tab MeasInterval (reference 1 p31) \tab - \tab \cr
#'
#' `samplingRate`\tab 512/(AvgInterval) (reference 1 p30; reference 4) \tab - #' \tab \cr
#'
#' }
#'
#' The `data` list contains items with names corresponding to `adp`
#' objects, with an exception for Nortek data.  Nortek instruments report some
#' things at a time interval that is longer than the velocity sampling, and
#' these are stored in `data` as `timeSlow`, `headingSlow`,
#' `pitchSlow`, `rollSlow`, and `temperatureSlow`; if burst
#' sampling was used, there will also be items `recordsBurst` and
#' `timeBurst`.
#'
#' The `processingLog` is in the standard format.
#'
#' @section Nortek files:
#'
#' **Sampling-rate and similar issues**
#'
#' The data format is inferred from the System Integrator Guide (reference 1A) and System
#' Integrator Manual (reference 1B).  These document lacks clarity in spots, and so
#' `read.adv.nortek` contains some assumptions that are noted here, so
#' that users will be aware of possible problems.
#'
#' A prominent example is the specification of the sampling rate, stored in
#' `metadata$sampingRate` in the return value.  Repeated examination of
#' the System Integrator Guide (reference 1) failed to indicate where this value is
#' stored in the various headers contained in Vector datasets.  After some
#' experimentation with a few data files, `read.adv.nortek` was set up to
#' calculate `metadata$samplingRate` as `512/AvgInterval` where
#' `AvgInterval` is a part of the ``User Configuration'' header (reference 1 p30),
#' where the explanation is ``average interval in seconds'').  This formula was
#' developed through trial and error, but it was confirmed later on the Nortek
#' discussion group, and it should appear in upcoming versions of (reference 1).
#'
#' Another basic issue is the determination of whether an instrument had
#' recorded in continuous mode or burst mode.  One might infer that
#' `TimCtrlReg` in the ``User Configuration'' header (reference 1 p30) determines
#' this, in bits 1 and 2.  However, this was the case in test files available
#' to the author.  For this reason, `read.adv.nortek` infers the mode by
#' reverse engineering of data files of known configuration.  The present
#' version of `read.adv.nortek` determines the sampling mode from the
#' ```NRecords`'' item of the ``Vector Velocity Data'' header, which seems
#' to be 0 for data collected continuously, and non-zero for data collected in
#' bursts.
#'
#' Taking these things together, we come upon the issue of how to infer
#' sampling times for Nortek instruments.  There do not seem to be definitive
#' documents on this, and so `read.adv.nortek` is based partly on
#' information (of unknown quality) found on Nortek discussion boards.  The
#' present version of `read.adv.nortek` infers the times of velocity
#' observations differently, depending on whether the instrument was set to
#' record in burst mode or continuous mode.  For burst mode, times stated in
#' the burst headers are used, but for continuous mode, times stated in the
#' ``vector system data'' are used.  On the advice found on a Nortek discussion
#' board, the burst-mode times are offset by 2 seconds to allow for the
#' instrument warm-up period.
#'
#' **Handling IMU (inertial measurement unit) data**
#'
#' Starting in March
#' 2016, `read.adv.nortek` has offered some support for handling IMU
#' (inertial measurement unit) data incorporated into Nortek binary files. This
#' is not described in the Nortek document named ``System Integrator Guide''
#' (reference 1A) but it appeared in ``System Integrator Manual'' (reference 1B;
#' reference 1C). Confusingly, 1B described 3 varieties of data, whereas 1C does not
#' describe any of these, but describes instead a fourth variety. As of March
#' 2016, `read.adv.nortek` handles all 4 varieties, because files in the
#' various schemes appear to exist. In `oce`, the varieties are named
#' after the byte code that flags them.  (Variety `c3` is the one
#' described in (reference 1C); the others were described in (reference 1B).)
#' The variety is stored in the `metadata` slot of the returned object as
#' a string named `IMUtype`.
#'
#' For each variety, the reader is cautioned that strong tests have not been
#' performed on the code.  One way to test the code is to compare with textual
#' data files produced by the Nortek software.  In March 2016, an `oce`
#' user shared a dataset of the `c3` variety, and this permitted detailed
#' comparison between the text file and the values inferred by
#' `read.adv.nortek`. The test suggested agreement (to within the
#' resolution printed in the text file) for velocity (`v` in the
#' `data` slot), signal amplitude (`a`), correlation (`q`),
#' pressure (`p`), the three components of IMU delta angle
#' (`IMUdeltaAngleX` etc), and all components of the rotation matrix
#' (`IMUrotation`).  However, the delta velocity signals did not match,
#' with `IMUdeltaVelocityX` disagreeing in the second decimal place,
#' `IMUdeltaVelocityY` component disagreeing in the first, and
#' `IMUdeltaVelocityZ` being out by a factor of about 10. This is github
#' issue 893 (\url{https://github.com/dankelley/oce/issues/893}).
#'
#' * Variety `c3` (signalled by byte 5 of a sequence being
#' `0xc3`) provides information on what Nortek calls DeltaAngle,
#' DeltaVelocity and Orientation Matrix. (Apart from the orientation matrix,
#' Nortek provides no documentation on what these quantities mean.) In the
#' object returned by `read.adv.nortek`, these are stored in the
#' `data` slot as `IMUdeltaAngleX`, `IMUdeltaAngleY`,
#' `IMUdeltaAngleZ`, `IMUdeltaVelocityX`, `IMUdeltaVelocityY`,
#' `IMUdeltaVelocityZ`, and `IMUrotation`, all vectors except the
#' last, which is a 3D array. In addition to these, `IMUtimestamp` is a
#' time-stamp, which is not defined in the Nortek documents but seems, from IMU
#' documents (reference 5), to be defined based on a clock that ticks once per 16
#' microseconds. Caution may be required in dealing with this timestamp, since
#' it seemed sensible in one test case (variety `d3`) but kept reseting to
#' zero in another (variety `c3`). The lack of Nortek documentation on
#' most of these quantities is a roadblock to implementing `oce` functions
#' dealing with IMU-enabled datasets
#'
#' * Variety `cc` (signalled by byte 5 of a sequence being
#' `0xcc`) provides information on acceleration, angular rotation rate,
#' magnetic vector and orientation matrix.  Each is a timeseries. Acceleration
#' is stored in the `data` slot as `IMUaccelX`, `IMUaccelY`,
#' `IMUaccelz`. The angular rotation components are `IMUngrtX`,
#' `IMUngrtY` and `IMUngrtz`. The magnetic data are in
#' `IMUmagrtx`, `IMUmagrty` and `IMUmagrtz`. Finally,
#' `IMUmatrix` is a rotation matrix made up from elements named
#' `M11`, `M12`, etc in the Nortek documentation.  In addition to all
#' of these, `IMUtime` stores time in seconds, with an origin whose
#' definition is not stated in reference 1B.
#'
#' * Variety `d2` (signalled by byte 5 being `0xd2`) provides
#' information on gyro-stabilized acceleration, angular rate and magnetometer
#' vectors.  The data stored `MUaccelX`, `IMUangrtX`,
#' `IMUmagrtX`, with similar for `Y` and `Z`.  Again, time is in
#' `IMUtime`. This data type has not been tested as of mid-March 2016,
#' because the developers do not have a test file with which to test.
#'
#' * Variety `d3` (signalled by byte 5 being `0xd3`) provides
#' information on DeltaAngle, DeltaVelocity and magnetometer vectors, stored in
#' `IMUdeltaAngleX`, `IMUdeltaVelocityX`, and
#' `IMUdeltaMagVectorX`, with similar for `Y` and `Z`. Again,
#' time is in `IMUtime`. This data type has not been tested as of
#' mid-March 2016, because the developers do not have a test file with which to
#' test.
#'
#' @author Dan Kelley
#'
#' @references
#'
#' 1A. Nortek AS.  System Integrator Guide (paradopp family of products). June
#' 2008.  (Doc No: PSI00-0101-0608).  (Users may find it helpful to also
#' examine newer versions of the guide.)
#'
#' 1B. Nortek AS.  System Integrator Manual. Dec 2014.
#' (`system-integrator-manual_Dec2014_jan.pdf`)
#'
#' 1C. Nortek AS.  System Integrator Manual. March 2016.
#' (`system-integrator-manual_Mar2016.pdf`)
#'
#' 2. SonTek/YSI ADVField/Hydra Acoustic Doppler Velocimeter (Field) Technical
#' Documentation (Sept 1, 2001).
#'
#' 3. Appendix 2.2.3 of the Sontek ADV operation Manual, Firmware Version 4.0
#' (Oct 1997).
#'
#' 4. Nortek Knowledge Center (http://www.nortekusa.com/en/knowledge-center)
#'
#' 5. A document describing an IMU unit that seems to be close to the one named
#' in (references 1B and C) as being an adjunct to Nortek Vector systems is at
#' `http://files.microstrain.com/3DM-GX3-35-Data-Communications-Protocol.pdf`
#'
#' @examples
#' \dontrun{
#' library(oce)
#' # A nortek Vector file
#' d <- read.oce("/data/archive/sleiwex/2008/moorings/m05/adv/nortek_1943/raw/adv_nortek_1943.vec",
#'               from=as.POSIXct("2008-06-26 00:00:00", tz="UTC"),
#'               to=as.POSIXct("2008-06-26 00:00:10", tz="UTC"))
#' plot(d, which=c(1:3,15))
#' }
#' @family things related to adv data

